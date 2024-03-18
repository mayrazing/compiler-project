from compiler.symtab import SymTab
from compiler.types import BasicType, Bool, FuncType, Int, Type, Unit
import compiler.ast as AST


def typecheck(node: AST.Module, symtab: SymTab[Type]) -> Type:

    def assign_type(node: AST.Expression, type: Type) -> Type:
        node.type = type
        return type

    def get_type_expr(node: AST.TypeExpr, symtab: SymTab) -> Type:

        def get_basic_type_expr(node: AST.BasicTypeExpr,
                                symtab: SymTab) -> BasicType:
            try:
                context_type = symtab.require(node.name)
            except ValueError:
                raise Exception(
                    f'{node.location}: unknown basic type expression "{node}"')

            if isinstance(context_type, BasicType):
                return context_type

            else:
                raise Exception(
                    f'{node.location}: basic type expression "{node}" can not matched with the type "{context_type}"'
                )

        def get_func_type_expr(node: AST.FuncTypeExpr,
                               symtab: SymTab) -> FuncType:

            args: list[BasicType] = []
            if node.params is not None:
                for param in node.params:
                    try:
                        arg_type = get_basic_type_expr(param, symtab)
                    except Exception:
                        raise Exception(
                            f'{node.location}: unknown argument type expression "{param}" in function type expression'
                        )

                    args.append(arg_type)

            return_type = get_basic_type_expr(node.result, symtab)

            if len(args) == 0:
                func_type = FuncType(None, return_type)
            else:
                func_type = FuncType(args, return_type)

            return func_type

        if isinstance(node, AST.BasicTypeExpr):
            return get_basic_type_expr(node, symtab)

        elif isinstance(node, AST.FuncTypeExpr):
            return get_func_type_expr(node, symtab)

        raise Exception(f'{node.location}: unknown type expression "{node}"')

    def typecheck_expression(node: AST.Expression, symtab: SymTab) -> Type:

        def typecheck_literal(node: AST.Literal, symtab: SymTab) -> Type:
            if isinstance(node.value, bool):
                return assign_type(node, Bool)
            elif isinstance(node.value, int):
                return assign_type(node, Int)
            elif node.value is None:
                return assign_type(node, Unit)
            else:
                raise Exception(
                    f'{node.location}: unknown type of literal "{node.value}"')

        def typecheck_identifier(node: AST.Identifier, symtab: SymTab) -> Type:
            try:
                value_type = symtab.require(node.name)
            except ValueError:
                raise Exception(
                    f'{node.location}: identifier "{node.name}" was not found in the context and all parent contexts'
                )

            return assign_type(node, value_type)

        def typecheck_unary_op(node: AST.UnaryOp, symtab: SymTab) -> Type:
            key_op: str = 'unary_' + node.op

            try:
                op_type = symtab.require(key_op)
            except ValueError:
                raise Exception(
                    f'{node.location}: unary operator "{key_op}" was not found in the context and all parent contexts'
                )

            clause_type = typecheck_expression(node.unary_clause, symtab)
            if op_type == clause_type:
                return assign_type(node, op_type)

            raise Exception(
                f'{node.location}: expected "{op_type}" type, got "{clause_type}"'
            )

        def typecheck_binary_op(node: AST.BinaryOp, symtab: SymTab) -> Type:
            exp_types = []
            exp_types.append(typecheck_expression(node.left, symtab))
            exp_types.append(typecheck_expression(node.right, symtab))

            if node.op == '=':
                if exp_types[0] == exp_types[1]:
                    return assign_type(node, exp_types[0])

                raise Exception(
                    f'{node.location}: operator "{node.op}" expected the same type on both sides, got "{exp_types[0]}" and "{exp_types[1]}"'
                )

            else:
                try:
                    op_type = symtab.require(node.op)
                except ValueError:
                    raise Exception(
                        f'{node.location}: binary operator "{node.op}" was not found in the context and all parent contexts'
                    )

                # if node.op in ['==', '!=']:
                #     if exp_types[0] in [Int, Bool
                #                         ] and exp_types[1] in [Int, Bool]:
                #         if exp_types[0] == exp_types[1]:
                #             return assign_type(node, Bool)

                #         raise Exception(
                #             f'{node.location}: operator "{node.op}" expected the same type on both sides, got "{exp_types[0]}" and "{exp_types[1]}"'
                #         )

                #     raise Exception(
                #         f'{node.location}: operator "{node.op}" expected either "Int" or "Bool" type on both sides, got "{exp_types[0]}" and "{exp_types[1]}"'
                #     )
                if isinstance(op_type, FuncType):
                    if op_type.args is not None:
                        for i, arg in enumerate(op_type.args):
                            types = [
                                globals()[type.strip()]
                                for type in arg.name.split('|')
                            ]

                            if exp_types[i] in types:
                                continue

                            raise Exception(
                                f'{node.location}: operator "{node.op}" expected the types "{op_type.args}", got "{exp_types[0]}" and "{exp_types[1]}"'
                            )

                    if node.op in ['==', '!='
                                   ] and exp_types[0] != exp_types[1]:
                        raise Exception(
                            f'{node.location}: operator "{node.op}" expected the same type on both sides, got "{exp_types[0]}" and "{exp_types[1]}"'
                        )

                    return assign_type(node, op_type.return_type)

                else:
                    raise Exception(
                        f'{node.location}: unknown type of operator "{node.op}"'
                    )

        def typecheck_block(node: AST.Block, symtab: SymTab) -> Type:
            context = SymTab({}, symtab)
            statements = node.statements

            if statements is None:
                return assign_type(node, Unit)

            value: Type = Unit
            for statement in statements:
                value = typecheck_expression(statement, context)

            return assign_type(node, value)

        def typecheck_var_declaration(node: AST.VarDeclaration,
                                      symtab: SymTab) -> Type:
            var_name: AST.Identifier = node.name
            """support shadow variables"""
            # try:
            #     var_value = symtab.require(var_name.name)
            # except ValueError:
            #     ...
            # raise Exception(
            #     f'{node.location}: variable "{var_name.name}" was defined in the context and all parent contexts'
            # )

            t_type = None
            if node.declared_type_exp is not None:
                t_type = get_type_expr(node.declared_type_exp, symtab)
                node.declared_type_exp.type = t_type

            exp_type = typecheck_expression(node.initializer, symtab)
            if t_type is not None and t_type != exp_type:
                raise Exception(
                    f'{node.location}: variable "{var_name.name}" expected type "{t_type}", got type "{exp_type}"'
                )

            symtab.locals[var_name.name] = exp_type
            return assign_type(node, Unit)

        def typecheck_if_expression(node: AST.IfExpression,
                                    symtab: SymTab) -> Type:
            t1 = typecheck_expression(node.cond, symtab)

            if t1 is not Bool:
                raise Exception(
                    f'{node.location}: if condition expression expected type "{Bool}", got type "{t1}"'
                )

            t2 = typecheck_expression(node.then_clause, symtab)
            if node.else_clause is None:
                return assign_type(node, Unit)

            t3 = typecheck_expression(node.else_clause, symtab)
            if t2 != t3:
                raise Exception(
                    f'{node.location}: if expression expected the same types on then clause and else clause, got "{t2}" and "{t3}"'
                )

            return assign_type(node, t2)

        def typecheck_while_loop(node: AST.WhileLoop, symtab: SymTab) -> Type:
            t1 = typecheck_expression(node.cond, symtab)

            if t1 is not Bool:
                raise Exception(
                    f'{node.location}: while-loop expression expected type "{Bool}", got type "{t1}"'
                )

            t2 = typecheck_expression(node.body, symtab)
            return assign_type(node, Unit)

        def typecheck_function_call(node: AST.FunctionCall,
                                    symtab: SymTab) -> Type:
            func_name: AST.Identifier = node.name

            try:
                func_type = symtab.require(func_name.name)
            except ValueError:
                raise Exception(
                    f'{node.location}: function "{func_name.name}" was not defined in the context and all parent contexts'
                )

            if not isinstance(func_type, FuncType):
                raise Exception(
                    f'{node.location}: type of function "{func_name.name}" expected "{FuncType}", got "{func_type}"'
                )

            elif node.args is None:
                if func_type.args is not None:
                    raise Exception(
                        f'{node.location}: function expression expected the number of parameters is "{len(func_type.args)}", got "0"'
                    )

            elif func_type.args is None:
                raise Exception(
                    f'{node.location}: function expression expected the number of parameters is "0", got "{len(node.args)}"'
                )

            else:
                num_args = len(node.args)
                if num_args > 6:
                    raise Exception(
                        f'{node.location}: number of parameters of the function "{func_name.name}" cannot exceed 6, got "{num_args}"'
                    )

                if num_args != len(func_type.args):
                    raise Exception(
                        f'{node.location}: function expression expected the number of parameters is "{len(func_type.args)}", got "{num_args}"'
                    )

                for i, arg in enumerate(node.args):
                    exp_value = typecheck_expression(arg, symtab)
                    if exp_value != func_type.args[i]:
                        raise Exception(
                            f'{node.location}: function expression expected the type of {i+1}-th parameter is "{func_type.args[i]}", got "{exp_value}"'
                        )

            return assign_type(node, func_type.return_type)

        def typecheck_return(node: AST.Return, symtab: SymTab) -> Type:
            if node.value is None:
                return assign_type(node, Unit)
            else:
                return typecheck_expression(node.value, symtab)

        match node:
            case AST.Literal():
                return typecheck_literal(node, symtab)

            case AST.Identifier():
                return typecheck_identifier(node, symtab)

            case AST.UnaryOp():
                return typecheck_unary_op(node, symtab)

            case AST.BinaryOp():
                return typecheck_binary_op(node, symtab)

            case AST.Block():
                return typecheck_block(node, symtab)

            case AST.VarDeclaration():
                return typecheck_var_declaration(node, symtab)

            case AST.IfExpression():
                return typecheck_if_expression(node, symtab)

            case AST.WhileLoop():
                return typecheck_while_loop(node, symtab)

            case AST.FunctionCall():
                return typecheck_function_call(node, symtab)

            case AST.Return():
                return typecheck_return(node, symtab)

            case AST.ControlFlow():
                return assign_type(node, Unit)

            case _:
                raise Exception(
                    f"{node.location}: unsupported AST expression node {node}")

    def typecheck_func_definition(node: AST.FuncDefinition,
                                  symtab: SymTab) -> Type:
        func_name: AST.Identifier = node.name

        try:
            symtab.require(func_name.name)
        except ValueError:
            args_type: list[BasicType] = []
            # check args types
            if node.args is not None:
                num_args = len(node.args)

                if num_args > 6:
                    raise Exception(
                        f'{node.location}: number of parameters of function definition "{func_name.name}" cannot exceed 6, got "{num_args}"'
                    )

                for arg_name, arg_type in node.args.items():
                    type_value = get_type_expr(arg_type, symtab)
                    arg_type.type = type_value

                    if isinstance(type_value, BasicType):
                        args_type.append(type_value)

                    symtab.add_local(arg_name.name, type_value)

            # check return type == body type
            try:
                r_type = get_type_expr(node.return_type, symtab)
            except Exception:
                raise Exception(
                    f'{node.location}: function definition "{func_name.name}" had a unknown return type "{r_type}"'
                )

            body_type = typecheck_expression(node.body, symtab)
            if r_type != body_type:
                raise Exception(
                    f'{node.location}: function definition "{func_name.name}" expected a return type "{r_type}", got "{body_type}"'
                )

            node.return_type.type = r_type
            if len(args_type) == 0:
                func_type = FuncType(None, r_type)
            else:
                func_type = FuncType(args_type, r_type)

            symtab.add_local(func_name.name, func_type)
            return Unit

        raise Exception(
            f'{node.location}: function definition "{func_name.name}" was defined in the context and all parent contexts'
        )

    def typecheck_module(node: AST.Module, symtab: SymTab) -> Type:
        if node.funcs is not None:
            for func_def in node.funcs:
                typecheck_func_definition(func_def, symtab)

        if node.expr is not None:
            return typecheck_expression(node.expr, symtab)

        return Unit

    """typecheck start here"""
    return typecheck_module(node, symtab)
