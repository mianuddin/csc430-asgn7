using Test

# Full project implemented.

#=
an ExprC is generally the input to evaluation, which may be one of the following:
 - numC: a real number
 - stringC: a string
 - idC: an identifier
 - ifC: a conditional statement consisting of test, then, and else clauses
 - appC: a function application
 - lamC: a function literal
=#

abstract type ExprC end

struct numC <: ExprC
    n::Float64
end

struct stringC <: ExprC
    s::String
end

struct idC <: ExprC
    id::Symbol
end

struct ifC <: ExprC
    test::ExprC
    then::ExprC
    elsee::ExprC
end

struct appC <: ExprC
    fun::ExprC
    args::Array{ExprC, 1}
end

struct lamC <: ExprC
    args::Array{Symbol, 1}
    body::ExprC
end

#=
a Value is generally the result of evaluation, which may be one of the following:
 - numV: a real number
 - boolV: a boolean
 - stringV : a string
 - closV: a closure
 - primV : a primitive operation
=#

abstract type Value end

struct numV <: Value
    n::Float64
end

struct boolV <: Value
    b::Bool
end

struct stringV <: Value
    s::String
end

struct primV <: Value
    o::Symbol
end

# a Binding represents a deferred substitution, which maps a name to a value
struct Binding
    name::Symbol
    val::Value
end

# Env represents a environment, which is a list of bindings
const Env = Array{Binding, 1}

struct closV <: Value
    args::Array{Symbol, 1}
    body::ExprC
    env::Env
end

# top_env contains default bindings for boolean literals and primitive operators
top_env = [
    Binding(Symbol("true"), boolV(true)),
    Binding(Symbol("false"), boolV(false)),
    Binding(:+, primV(:+)),
    Binding(:-, primV(:-)),
    Binding(:*, primV(:*)),
    Binding(:/, primV(:/)),
    Binding(:<=, primV(:<=)),
    Binding(:equal_huh, primV(:equal_huh)),
]

# retrieves a value from an environment given a symbol
function lookup(s::Symbol, e::Env)::Value
    has_name_s_huh = x -> s === x.name
    i = findfirst(has_name_s_huh, e)
    if i != nothing
        return e[i].val
    else
        throw(ErrorException("DUNQ: reference to unbound id"))
    end
end

# eagerly binds arg names to the values passed to the function and returns the resulting environment
function args_bind(interpd_app_args::Array{Value, 1}, fun_args::Array{Symbol, 1}, env::Env)
    if length(interpd_app_args) == length(fun_args)
        return map(Binding, fun_args, interpd_app_args)
    else
        throw(ErrorException("DUNQ: function called with wrong arity"))
    end
end

# evaluates an ExprC to a value
function interp(e::ExprC, env::Env)::Value
    if typeof(e) == numC
        return numV(e.n)
    elseif typeof(e) == stringC
        return stringV(e.s)
    elseif typeof(e) == idC
        return lookup(e.id, env)
    elseif typeof(e) == ifC
        interpd_test = interp(e.test, env)
        if typeof(interpd_test) != boolV
            throw(ErrorException("DUNQ: test doesn't evaluate to a boolean"))
        else
            return interp(if interpd_test == boolV(true) e.then else e.elsee end, env)
        end
    elseif typeof(e) == lamC
        return closV(e.args, e.body, env)
    elseif typeof(e) == appC
        interpd_fun = interp(e.fun, env)
        interpd_args::Array{Value, 1} = map(a -> interp(a, env), e.args)
        if typeof(interpd_fun) == primV
            if length(interpd_args) != 2
                throw(ErrorException("DUNQ: primop requres exactly two args"))
            end

            if :+ === interpd_fun.o
                return numV(interpd_args[1].n + interpd_args[2].n)
            elseif :- === interpd_fun.o
                return numV(interpd_args[1].n - interpd_args[2].n)
            elseif :* === interpd_fun.o
                return numV(interpd_args[1].n * interpd_args[2].n)
            elseif :/ === interpd_fun.o
                if interpd_args[2].n == 0
                    throw(ErrorException("DUNQ: divide by zero"))
                end
                return numV(interpd_args[1].n / interpd_args[2].n)
            elseif :<= === interpd_fun.o
                return boolV(interpd_args[1].n <= interpd_args[2].n)
            elseif :equal_huh === interpd_fun.o
                if typeof(interpd_args[1]) == numV && typeof(interpd_args[2]) == numV
                    return boolV(interpd_args[1].n == interpd_args[2].n)
                elseif typeof(interpd_args[1]) == stringV && typeof(interpd_args[2]) == stringV
                    return boolV(interpd_args[1].s == interpd_args[2].s)
                elseif typeof(interpd_args[1]) == boolV && typeof(interpd_args[2]) == boolV
                    return boolV(interpd_args[1].b == interpd_args[2].b)
                else
                    return boolV(false)
                end
            else
                throw(ErrorException("DUNQ: unknown primop specified"))
            end
        elseif typeof(interpd_fun) == closV
            return interp(interpd_fun.body, vcat(args_bind(interpd_args, interpd_fun.args, env), interpd_fun.env))
        else
            throw(ErrorException("DUNQ: application doesn't eval to a function or primop"))
        end
    end
end

# parse array into ExprC
function parsee(s::Any)::ExprC
    if typeof(s) == Float64
        return numC(s)
    elseif typeof(s) == String
        return stringC(s)
    elseif typeof(s) == Symbol
        return idC(s) # not checking if reserved
    elseif typeof(s) == Array{Any, 1}
        if s[1] === :if
            parsed_args = map(arg -> parsee(arg), s[2:end])
            try
                return ifC(parsed_args[1], parsed_args[2], parsed_args[3])
            catch
                throw(ErrorException("DUNQ: improper if format"))
            end
      	# broken atm
        elseif array[1] === :lam
            if typeof(array[2]) == Array{Symbol, 1}
                parsed_args = map(arg -> parsee(arg), array[2])
                try
                    return lamC(parsed_args, parse(array[3]))
                catch
                    throw(ErrorException("DUNQ: improper lambda format"))
                end
            else
                throw(ErrorException("DUNQ: improper lambda format"))
            end
        #=
        elseif array[1] === :vars
            # vars
        =#
        end
    else
        throw(ErrorException("DUNQ: Unknown sexp given"))
    end
end

# returns the string representation of any DUNQ3 value
function serialize(v::Value)::String
    if typeof(v) == numV
        return "$(v.n)"
    elseif typeof(v) == boolV
        return "$(v.b)"
    elseif typeof(v) == stringV
        return "\"$(v.s)\""
    elseif typeof(v) == primV
        return "#<primop>"
    elseif typeof(v) ==closV
        return "#<procedure>"
    else
        throw(ErrorException("DUNQ: encountered unknown value"))
    end
end

# parses a given Sexp containing a list of functions, and evaluates it to a value
function top_interp(s::Any)::String
    serialize(interp(parsee(s),top_env))
end

# top_env Tests
@test typeof(top_env) == Env

# lookup Tests
@test lookup(Symbol("true"), top_env) == boolV(true)
@test lookup(:+, top_env) == primV(:+)
@test lookup(:equal_huh, top_env) == primV(:equal_huh)
@test_throws ErrorException lookup(:garbage, top_env)

# interp Tests
@test interp(numC(0.0), top_env) == numV(0.0)
@test interp(stringC("Hello, world!"), top_env) == stringV("Hello, world!")
@test interp(idC(Symbol("true")), top_env) == boolV(true)
@test interp(idC(Symbol("false")), top_env) == boolV(false)
@test interp(appC(idC(:<=), [numC(1.0), numC(2.0)]), top_env) == boolV(true)
@test interp(appC(idC(:<=), [numC(4.0), numC(2.0)]), top_env) == boolV(false)
@test interp(
    ifC(appC(idC(:<=), [numC(1.0), numC(2.0)]), numC(6.0), numC(1.0)),
    top_env
) == numV(6.0)
@test interp(
    ifC(appC(idC(:equal_huh), [idC(Symbol("true")), idC(Symbol("true"))]),
    idC(Symbol("true")), idC(Symbol("false"))),
    top_env
) == boolV(true)
@test interp(
    ifC(appC(idC(:equal_huh), [idC(Symbol("true")), idC(Symbol("false"))]),
    idC(Symbol("true")), idC(Symbol("false"))),
    top_env
) == boolV(false)
@test interp(
    ifC(appC(idC(:equal_huh), [stringC("d"), stringC("d")]),
    idC(Symbol("true")), idC(Symbol("false"))),
    top_env
) == boolV(true)
@test interp(
    ifC(appC(idC(:equal_huh), [stringC("abc"), stringC("d")]),
    idC(Symbol("true")), idC(Symbol("false"))),
    top_env
) == boolV(false)
@test interp(
    ifC(appC(idC(:equal_huh), [numC(1.0), numC(1.0)]),
    idC(Symbol("true")), idC(Symbol("false"))),
    top_env
) == boolV(true)
@test interp(
    ifC(appC(idC(:equal_huh), [numC(1.0), numC(2.0)]),
    idC(Symbol("true")), idC(Symbol("false"))),
    top_env
) == boolV(false)
@test interp(
    ifC(appC(idC(:equal_huh), [numC(1.0), stringC("2")]),
    idC(Symbol("true")), idC(Symbol("false"))),
    top_env
) == boolV(false)
@test interp(
    ifC(appC(idC(:equal_huh), [idC(Symbol("true")), numC(2.0)]),
    idC(Symbol("true")), idC(Symbol("false"))),
    top_env
) == boolV(false)
@test interp(
    ifC(appC(idC(:equal_huh), [idC(Symbol("false")), stringC("d")]),
    idC(Symbol("true")), idC(Symbol("false"))),
    top_env
) == boolV(false)
test_closure_1 = interp(lamC([], numC(0.0)), top_env)
@test typeof(test_closure_1) == closV
@test test_closure_1.args == []
@test test_closure_1.body == numC(0.0)
@test test_closure_1.env == top_env
test_closure_2 = interp(lamC([:x], idC(:x)), top_env)
@test typeof(test_closure_2) == closV
@test test_closure_2.args == [:x]
@test test_closure_2.body == idC(:x)
@test test_closure_2.env == top_env
@test interp(appC(idC(:+), [numC(2.0), numC(1.0)]), top_env) == numV(3.0)
@test interp(appC(idC(:-), [numC(2.0), numC(1.0)]), top_env) == numV(1.0)
@test interp(appC(idC(:*), [numC(2.0), numC(1.0)]), top_env) == numV(2.0)
@test interp(appC(idC(:/), [numC(2.0), numC(1.0)]), top_env) == numV(2.0)
@test interp(appC(idC(:<=), [numC(2.0), numC(1.0)]), top_env) == boolV(false)
@test interp(appC(lamC([:x], appC(idC(:+), [numC(1.0), idC(:x)])), [numC(2.0)]), top_env) == numV(3.0)
fib_prog = appC(lamC([:fib], appC(idC(:equal_huh), [numC(21.0), appC(idC(:fib), [idC(:fib), numC(8.0)])])),
                [lamC([:f, :n], ifC(appC(idC(:<=), [idC(:n), numC(1.0)]),
                                    idC(:n),
                                    appC(idC(:+), [appC(idC(:f), [idC(:f), appC(idC(:-), [idC(:n), numC(1.0)])]),
                                                    appC(idC(:f), [idC(:f), appC(idC(:-), [idC(:n), numC(2.0)])])])))])
@test interp(fib_prog, top_env) == boolV(true)
@test_throws ErrorException interp(appC(idC(:+), [numC(1.0)]), top_env)
@test_throws ErrorException interp(appC(idC(:/), [numC(1.0), numC(0.0)]), top_env)
@test_throws ErrorException interp(appC(idC(:%), [numC(1.0), numC(1.0)]), top_env)
@test_throws ErrorException interp(appC(idC(Symbol("true")), [numC(1.0), numC(1.0)]), top_env)

# parsee Tests
@test parsee(1.0) == numC(1.0)
@test parsee("hello") == stringC("hello")
@test parsee(:sym) == idC(:sym)
@test parsee([:if, Symbol("true"), 1.0, 0.0]) == ifC(idC(Symbol("true")), numC(1.0), numC(0.0))

# serialize Tests
@test serialize(numV(0.0)) == "0.0"
@test serialize(boolV(true)) == "true"
@test serialize(boolV(false)) == "false"
@test serialize(stringV("Hello, world!")) == "\"Hello, world!\""
@test serialize(primV(:+)) == "#<primop>"
@test serialize(closV([:f], idC(:hello), top_env)) == "#<procedure>"

# top-interp Tests
@test top_interp(1.0) == "1.0"
@test top_interp(Symbol("true")) == "true"
@test top_interp(Symbol("false")) == "false"
@test top_interp([:if, Symbol("true"), 1., 0.]) == "1.0"
@test top_interp([:if, Symbol("false"), 1., 0.]) == "0.0"
@test top_interp("Hello, world!") == "\"Hello, world!\""
