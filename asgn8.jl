using Test

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
    n::Real
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
    n::Real
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

@test typeof(top_env) == Env

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

@test lookup(Symbol("true"), top_env) == boolV(true)
@test lookup(:+, top_env) == primV(:+)
@test lookup(:equal_huh, top_env) == primV(:equal_huh)
@test_throws ErrorException lookup(:garbage, top_env)

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

@test interp(numC(0), top_env) == numV(0)
@test interp(stringC("Hello, world!"), top_env) == stringV("Hello, world!")
@test interp(idC(Symbol("true")), top_env) == boolV(true)
@test interp(idC(Symbol("false")), top_env) == boolV(false)
test_closure_1 = interp(lamC([], numC(0)), top_env)
@test typeof(test_closure_1) == closV
@test test_closure_1.args == []
@test test_closure_1.body == numC(0)
@test test_closure_1.env == top_env
test_closure_2 = interp(lamC([:x], idC(:x)), top_env)
@test typeof(test_closure_2) == closV
@test test_closure_2.args == [:x]
@test test_closure_2.body == idC(:x)
@test test_closure_2.env == top_env
@test interp(appC(idC(:+), [numC(2), numC(1)]), top_env) == numV(3)
@test interp(appC(idC(:-), [numC(2), numC(1)]), top_env) == numV(1)
@test interp(appC(idC(:*), [numC(2), numC(1)]), top_env) == numV(2)
@test interp(appC(idC(:/), [numC(2), numC(1)]), top_env) == numV(2.0)
@test interp(appC(idC(:<=), [numC(2), numC(1)]), top_env) == boolV(false)
@test interp(appC(lamC([:x], appC(idC(:+), [numC(1), idC(:x)])), [numC(2)]), top_env) == numV(3)
@test_throws ErrorException interp(appC(idC(:+), [numC(1)]), top_env)
@test_throws ErrorException interp(appC(idC(:/), [numC(1), numC(0)]), top_env)
@test_throws ErrorException interp(appC(idC(:%), [numC(1), numC(1)]), top_env)
@test_throws ErrorException interp(appC(idC(Symbol("true")), [numC(1), numC(1)]), top_env)

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
    else
        throw(ErrorException("DUNQ: encountered unknown value"))
    end
end

@test serialize(numV(0)) == "0"
@test serialize(boolV(true)) == "true"
@test serialize(boolV(false)) == "false"
@test serialize(primV(:+)) == "#<primop>"
