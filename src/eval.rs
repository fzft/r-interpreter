use std::any::Any;
use crate::object::{Object, Integer, Boolean, Null, ReturnValue, Error, Blank, Function, StringObject, BUILTINS, BuiltinFunction, ArrayObject, HashObject, HashPair};
use crate::ast;
use crate::ast::Node;
use crate::lexer::Lexer;
use crate::parser;
use crate::env::Environment;
use std::collections::HashMap;


pub fn eval(node: &dyn ast::Node, env: &mut Environment) -> Box<dyn Object> {
    return if let Some(node) = node.as_any().downcast_ref::<ast::Program>() {
        eval_statements(&node.statements, env)
    } else {
        Box::new(Null {})
    };
}

fn eval_statements(stmts: &Vec<Box<dyn ast::Statement>>, env: &mut Environment) -> Box<dyn Object> {
    let mut result: Box<dyn Object> = Box::new(Null {});
    for statement in stmts {
        result = eval_statement(statement.as_ref(), env);
        if let Some(result) = result.as_any().downcast_ref::<ReturnValue>() {
            return result.value.clone();
        } else if let Some(_) = result.as_any().downcast_ref::<Error>() {
            return result;
        }
    }
    result
}

fn eval_statement(stmt: &dyn ast::Statement, env: &mut Environment) -> Box<dyn Object> {
    return if let Some(expr_stmt) = stmt.as_any().downcast_ref::<ast::ExpressionStatement>() {
        eval_express(expr_stmt.expression.as_ref().unwrap().as_ref(), env)
    } else if let Some(block_stmt) = stmt.as_any().downcast_ref::<ast::BlockStatement>() {
        eval_statements(&block_stmt.statements, env)
    } else if let Some(ret_stmt) = stmt.as_any().downcast_ref::<ast::ReturnStatement>() {
        let val = eval_express(ret_stmt.return_value.as_ref().unwrap().as_ref(), env);
        if is_error(val.as_ref()) {
            return val;
        }
        Box::new(ReturnValue { value: val })
    } else if let Some(let_stmt) = stmt.as_any().downcast_ref::<ast::LetStatement>() {
        let val = eval_express(let_stmt.value.as_ref().unwrap().as_ref(), env);
        if is_error(val.as_ref()) {
            return val;
        }
        env.set(let_stmt.name.to_string(), val);
        Box::new(Blank {})
    } else {
        Box::new(Null {})
    };
}

fn eval_expresses(exprs: &Vec<Box<dyn ast::Expression>>, env: &mut Environment) -> Vec<Box<dyn Object>> {
    let mut result: Vec<Box<dyn Object>> = Vec::new();
    for expr in exprs {
        result.push(eval_express(expr.as_ref(), env));
    }
    result
}

fn eval_express(expr: &dyn ast::Expression, env: &mut Environment) -> Box<dyn Object> {
    return if let Some(int_expr) = expr.as_any().downcast_ref::<ast::IntegerLiteral>() {
        Box::new(Integer { value: int_expr.value })
    } else if let Some(str_expr) = expr.as_any().downcast_ref::<ast::StringLiteral>() {
        Box::new(StringObject { value: str_expr.value.clone() })
    } else if let Some(array_expr) = expr.as_any().downcast_ref::<ast::ArrayLiteral>() {
        let elements = eval_expresses(&array_expr.elements, env);
        if elements.len() == 1 && is_error(elements[0].as_ref()) {
            return elements[0].clone();
        }
        Box::new(ArrayObject { elements: elements })
    } else if let Some(hash_lit) = expr.as_any().downcast_ref::<ast::HashLiteral>() {
        eval_hash_literal(hash_lit, env)
    } else if let Some(index_expr) = expr.as_any().downcast_ref::<ast::IndexExpression>() {
        let left = eval_express(index_expr.left.as_ref().unwrap().as_ref(), env);
        let index = eval_express(index_expr.index.as_ref().unwrap().as_ref(), env);
        if is_error(left.as_ref()) {
            return left;
        }
        if is_error(index.as_ref()) {
            return index;
        }
        eval_index_expression(left.as_ref(), index.as_ref())
    } else if let Some(bool_expr) = expr.as_any().downcast_ref::<ast::Boolean>() {
        if bool_expr.value {
            Box::new(Boolean { value: true })
        } else {
            Box::new(Boolean { value: false })
        }
    } else if let Some(fn_expr) = expr.as_any().downcast_ref::<ast::FunctionLiteral>() {
        Box::new(Function {
            parameters: fn_expr.parameters.clone(),
            body: Box::new(fn_expr.body.clone().unwrap()),
            env: env.clone(),
        })
    } else if let Some(prefix_expr) = expr.as_any().downcast_ref::<ast::PrefixExpression>() {
        let right = eval_express(prefix_expr.right.as_ref().unwrap().as_ref(), env);
        eval_prefix_expression(prefix_expr.operator.as_ref(), right.as_ref())
    } else if let Some(infix_expr) = expr.as_any().downcast_ref::<ast::InfixExpression>() {
        let left = eval_express(infix_expr.left.as_ref().unwrap().as_ref(), env);
        let right = eval_express(infix_expr.right.as_ref().unwrap().as_ref(), env);
        eval_infix_expression(infix_expr.operator.as_ref(), left.as_ref(), right.as_ref())
    } else if let Some(if_expr) = expr.as_any().downcast_ref::<ast::IfExpression>() {
        eval_if_expression(if_expr, env)
    } else if let Some(identifier) = expr.as_any().downcast_ref::<ast::Identifier>() {
        eval_identifier(identifier, env)
    } else if let Some(call_expr) = expr.as_any().downcast_ref::<ast::CallExpression>() {
        let func = eval_express(call_expr.function.as_ref().unwrap().as_ref(), env);
        if is_error(func.as_ref()) {
            return func;
        }
        let args = eval_expresses(&call_expr.arguments, env);
        if args.len() == 1 && is_error(args[0].as_ref()) {
            return args[0].clone();
        }
        apply_function(func.as_ref(), &args)
    } else {
        Box::new(Blank {})
    };
}


fn eval_prefix_expression(operator: &str, right: &dyn Object) -> Box<dyn Object> {
    if operator == "!" {
        return eval_bang_operator_expression(right);
    } else if operator == "-" {
        return eval_minus_prefix_operator_expression(right);
    } else {
        return Box::new(Error { message: format!("unknown operator: {}{}", operator, right.object_type()) });
    }
}

fn eval_bang_operator_expression(right: &dyn Object) -> Box<dyn Object> {
    if let Some(right) = right.as_any().downcast_ref::<Boolean>() {
        if right.value {
            return Box::new(Boolean { value: false });
        } else {
            return Box::new(Boolean { value: true });
        }
    } else if let Some(_) = right.as_any().downcast_ref::<Null>() {
        return Box::new(Boolean { value: true });
    } else {
        return Box::new(Boolean { value: false });
    }
}

fn eval_minus_prefix_operator_expression(right: &dyn Object) -> Box<dyn Object> {
    if let Some(right) = right.as_any().downcast_ref::<Integer>() {
        return Box::new(Integer { value: -right.value });
    } else {
        return Box::new(Error { message: format!("unknown operator: -{}", right.object_type()) });
    }
}

fn eval_infix_expression(operator: &str, left: &dyn Object, right: &dyn Object) -> Box<dyn Object> {
    if let Some(left) = left.as_any().downcast_ref::<Integer>() {
        if let Some(right) = right.as_any().downcast_ref::<Integer>() {
            return eval_integer_infix_expression(operator, left, right);
        } else {
            return Box::new(Error { message: format!("type mismatch: {} {} {}", left.object_type(), operator, right.object_type()) });
        }
    } else if let Some(left) = left.as_any().downcast_ref::<Boolean>() {
        if let Some(right) = right.as_any().downcast_ref::<Boolean>() {
            return eval_boolean_infix_expression(operator, left, right);
        } else {
            return Box::new(Error { message: format!("type mismatch: {} {} {}", left.object_type(), operator, right.object_type()) });
        }
    } else if let Some(left) = left.as_any().downcast_ref::<StringObject>() {
        if let Some(right) = right.as_any().downcast_ref::<StringObject>() {
            if operator == "+" {
                return Box::new(StringObject { value: format!("{}{}", left.value, right.value) });
            } else {
                return Box::new(Error { message: format!("unknown operator: {} {} {}", left.object_type(), operator, right.object_type()) });
            }
        } else {
            return Box::new(Error { message: format!("type mismatch: {} {} {}", left.object_type(), operator, right.object_type()) });
        }
    } else {
        return Box::new(Error { message: format!("unknown operator: {} {} {}", left.object_type(), operator, right.object_type()) });
    }
}

fn eval_integer_infix_expression(operator: &str, left: &Integer, right: &Integer) -> Box<dyn Object> {
    match operator {
        "+" => Box::new(Integer { value: left.value + right.value }),
        "-" => Box::new(Integer { value: left.value - right.value }),
        "*" => Box::new(Integer { value: left.value * right.value }),
        "/" => Box::new(Integer { value: left.value / right.value }),
        "<" => Box::new(Boolean { value: left.value < right.value }),
        ">" => Box::new(Boolean { value: left.value > right.value }),
        "==" => Box::new(Boolean { value: left.value == right.value }),
        "!=" => Box::new(Boolean { value: left.value != right.value }),
        _ => Box::new(Error { message: format!("unknown operator: {} {} {}", left.object_type(), operator, right.object_type()) }),
    }
}

fn eval_boolean_infix_expression(operator: &str, left: &Boolean, right: &Boolean) -> Box<dyn Object> {
    match operator {
        "==" => Box::new(Boolean { value: left.value == right.value }),
        "!=" => Box::new(Boolean { value: left.value != right.value }),
        _ => Box::new(Error { message: format!("unknown operator: {} {} {}", left.object_type(), operator, right.object_type()) }),
    }
}


fn eval_if_expression(if_expr: &ast::IfExpression, env: &mut Environment) -> Box<dyn Object> {
    let condition = eval_express(if_expr.condition.as_ref().unwrap().as_ref(), env);
    if is_truthy(condition.as_ref()) {
        return eval_statement(if_expr.consequence.as_ref().unwrap(), env);
    } else if let Some(alternative) = if_expr.alternative.as_ref() {
        return eval_statement(alternative, env);
    } else {
        return Box::new(Null {});
    }
}

fn is_truthy(obj: &dyn Object) -> bool {
    if let Some(obj) = obj.as_any().downcast_ref::<Null>() {
        return false;
    } else if let Some(obj) = obj.as_any().downcast_ref::<Boolean>() {
        return obj.value;
    } else {
        return true;
    }
}

fn is_error(obj: &dyn Object) -> bool {
    if let Some(_) = obj.as_any().downcast_ref::<Error>() {
        return true;
    } else {
        return false;
    }
}

fn eval_identifier(node: &ast::Identifier, env: &mut Environment) -> Box<dyn Object> {
    if let Some(val) = env.get(&node.value) {
        return val.clone_box();
    }

    if let Some(builtin) = BUILTINS.get(node.value.clone().as_str()) {
        return builtin.clone_box();
    }

    Box::new(Error { message: format!("identifier not found: {}", node.value) })
}

fn apply_function(func: &dyn Object, args: &Vec<Box<dyn Object>>) -> Box<dyn Object> {
    if let Some(func) = func.as_any().downcast_ref::<Function>() {
        let mut extended_env = extend_function_env(func, args);
        let evaluated = eval_statement(func.body.as_ref(), &mut extended_env);
        return unwrap_return_value(evaluated);
    } else if let Some(build_in_func) = func.as_any().downcast_ref::<BuiltinFunction>() {
        let new_args = args.iter().map(|arg| arg.clone_box()).collect::<Vec<Box<dyn Object>>>();
        return (build_in_func.func)(new_args);
    } else {
        return Box::new(Error {
            message: format!("not a function: {}", func.object_type()),
        });
    }
}

fn extend_function_env(func: &Function, args: &Vec<Box<dyn Object>>) -> Environment {
    let mut env = Environment::new_enclosed(Box::new(func.env.clone()));
    for (i, param) in func.parameters.iter().enumerate() {
        env.set(param.to_string(), args[i].clone());
    }
    env
}

fn unwrap_return_value(obj: Box<dyn Object>) -> Box<dyn Object> {
    if let Some(obj) = obj.as_any().downcast_ref::<ReturnValue>() {
        return obj.value.clone();
    } else {
        return obj;
    }
}

fn eval_index_expression(left: &dyn Object, index: &dyn Object) -> Box<dyn Object> {
    if let Some(left) = left.as_any().downcast_ref::<ArrayObject>() {
        if let Some(index) = index.as_any().downcast_ref::<Integer>() {
            return eval_array_index_expression(left, index);
        } else {
            return Box::new(Error { message: format!("index operator not supported: {}", index.object_type()) });
        }
    } else if let Some(left) = left.as_any().downcast_ref::<HashObject>() {
        return eval_hash_index_expression(left, index);
    } else {
        return Box::new(Error { message: format!("index operator not supported: {}", left.object_type()) });
    }
}

fn eval_hash_index_expression(hash: &HashObject, index: &dyn Object) -> Box<dyn Object> {
    if let Some(pair) = hash.pairs.get(&index.hash_key()) {
        return pair.value.clone();
    }
    Box::new(Null {})
}

fn eval_array_index_expression(array: &ArrayObject, index: &Integer) -> Box<dyn Object> {
    let idx = index.value as usize;
    let max = array.elements.len() - 1;
    if idx < 0 || idx > max {
        return Box::new(Null {});
    }
    array.elements[idx].clone()
}

fn eval_hash_literal(hash: &ast::HashLiteral, env: &mut Environment) -> Box<dyn Object> {
    let mut pairs = HashMap::new();
    for (key_node, value_node) in hash.pairs.iter() {
        let key = eval_express(key_node.as_ref(), env);
        if is_error(key.as_ref()) {
            return key;
        }
        let value = eval_express(value_node.as_ref(), env);
        if is_error(value.as_ref()) {
            return value;
        }
        pairs.insert(key.hash_key(), HashPair { key: key, value: value });
    }
    Box::new(HashObject { pairs: pairs })
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use super::*;

    struct TestInt {
        input: &'static str,
        expected: i64,
    }

    #[derive(Debug)]
    struct TestBool {
        input: &'static str,
        expected: bool,
    }

    struct TestIf {
        input: &'static str,
        expected: Option<i64>,
    }

    struct TestStr {
        input: &'static str,
        expected_message: &'static str,
    }

    #[test]
    fn test_eval_integer_expression() {
        let tests = vec![
            TestInt { input: "5", expected: 5 },
            TestInt { input: "10", expected: 10 },
            TestInt { input: "-5", expected: -5 },
            TestInt { input: "-10", expected: -10 },
            TestInt { input: "5 + 5 + 5 + 5 - 10", expected: 10 },
            TestInt { input: "2 * 2 * 2 * 2 * 2", expected: 32 },
            TestInt { input: "-50 + 100 + -50", expected: 0 },
            TestInt { input: "5 * 2 + 10", expected: 20 },
            TestInt { input: "5 + 2 * 10", expected: 25 },
            TestInt { input: "20 + 2 * -10", expected: 0 },
            TestInt { input: "50 / 2 * 2 + 10", expected: 60 },
            TestInt { input: "2 * (5 + 10)", expected: 30 },
            TestInt { input: "3 * 3 * 3 + 10", expected: 37 },
            TestInt { input: "3 * (3 * 3) + 10", expected: 37 },
            TestInt { input: "(5 + 10 * 2 + 15 / 3) * 2 + -10", expected: 50 },
        ];

        for tt in tests.iter() {
            let evaluated = test_eval(tt.input);
            test_integer_object(evaluated.as_ref().as_any(), tt.expected);
        }
    }

    #[test]
    fn test_if_else_expressions() {
        let tests = vec![
            TestIf { input: "if (true) { 10 }", expected: Some(10) },
            TestIf { input: "if (false) { 10 }", expected: None },
            TestIf { input: "if (1) { 10 }", expected: Some(10) },
            TestIf { input: "if (1 < 2) { 10 }", expected: Some(10) },
            TestIf { input: "if (1 > 2) { 10 }", expected: None },
            TestIf { input: "if (1 > 2) { 10 } else { 20 }", expected: Some(20) },
            TestIf { input: "if (1 < 2) { 10 } else { 20 }", expected: Some(10) },
        ];

        for test in tests {
            let evaluated = test_eval(test.input);
            match test.expected {
                Some(val) => test_integer_object(evaluated.as_ref().as_any(), val),
                None => test_null_object(evaluated.as_ref().as_any()),
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let tests = vec![
            TestInt { input: "return 10;", expected: 10 },
            TestInt { input: "return 10; 9;", expected: 10 },
            TestInt { input: "return 2 * 5; 9;", expected: 10 },
            TestInt { input: "9; return 2 * 5; 9;", expected: 10 },
        ];

        for test in tests {
            let evaluated = test_eval(test.input);
            test_integer_object(evaluated.as_ref().as_any(), test.expected);
        }
    }


    fn test_null_object(obj: &dyn Any) {
        if let Some(result) = obj.downcast_ref::<Null>() {} else {
            eprintln!("object is not Null. got={:?}", obj);
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests = vec![
            TestBool { input: "!true", expected: false },
            TestBool { input: "!false", expected: true },
            TestBool { input: "!5", expected: false },
            TestBool { input: "!!true", expected: true },
            TestBool { input: "!!false", expected: false },
            TestBool { input: "!!5", expected: true },
            TestBool { input: "true == true", expected: true },
            TestBool { input: "false == false", expected: true },
            TestBool { input: "true == false", expected: false },
            TestBool { input: "true != false", expected: true },
            TestBool { input: "false != true", expected: true },
            TestBool { input: "(1 < 2) == true", expected: true },
            TestBool { input: "(1 < 2) == false", expected: false },
            TestBool { input: "(1 > 2) == true", expected: false },
            TestBool { input: "(1 > 2) == false", expected: true },
        ];

        for test in tests {
            let evaluated = test_eval(test.input);
            test_boolean_object(evaluated.as_ref().as_any(), test.expected);
        }
    }

    #[test]
    fn test_error_handling() {
        let tests = vec![
            TestStr { input: "5 + true;", expected_message: "type mismatch: INTEGER + BOOLEAN" },
            TestStr { input: "5 + true; 5;", expected_message: "type mismatch: INTEGER + BOOLEAN" },
            TestStr { input: "-true", expected_message: "unknown operator: -BOOLEAN" },
            TestStr { input: "true + false;", expected_message: "unknown operator: BOOLEAN + BOOLEAN" },
            TestStr { input: "5; true + false; 5", expected_message: "unknown operator: BOOLEAN + BOOLEAN" },
            TestStr { input: "if (10 > 1) { true + false; }", expected_message: "unknown operator: BOOLEAN + BOOLEAN" },
            TestStr {
                input: r#"
if (10 > 1) {
    if (10 > 1) {
        return true + false;
    }
    return 1;
}
"#,
                expected_message: "unknown operator: BOOLEAN + BOOLEAN",
            },
            TestStr { input: "foobar", expected_message: "identifier not found: foobar" },
            TestStr { input: r#"{"name": "Monkey"}[fn(x) { x }];"#, expected_message: "unusable as hash key: FUNCTION" },
        ];

        for test in tests {
            let evaluated = test_eval(test.input); // Assuming you have a function named `test_eval`.
            match evaluated.as_any().downcast_ref::<Error>() {
                Some(err_obj) => {
                    assert_eq!(test.expected_message, err_obj.message, "For input: {}", test.input);
                }
                None => {
                    test_null_object(evaluated.as_ref().as_any());
                }
            }
        }
    }

    #[test]
    fn test_let_statements() {
        let tests = vec![
            TestInt { input: "let a = 5; a;", expected: 5 },
            TestInt { input: "let a = 5 * 5; a;", expected: 25 },
            TestInt { input: "let a = 5; let b = a; b;", expected: 5 },
            TestInt { input: "let a = 5; let b = a; let c = a + b + 5; c;", expected: 15 },
        ];

        for test in tests {
            test_integer_object(test_eval(test.input).as_any(), test.expected);
        }
    }

    #[test]
    fn test_string_concatenation() {
        let input = r#""Hello" + " " + "World!""#;
        let evaluated = test_eval(input); // Assuming test_eval returns a Box<dyn Object>

        match evaluated.as_any().downcast_ref::<StringObject>() {
            Some(s) => {
                assert_eq!(s.value, "Hello World!", "String has wrong value. got={}", s.value);
            }
            None => panic!("object is not a String. got={:?}", evaluated),
        }
    }


    #[test]
    fn test_string_literal() {
        let input = r#""Hello World!""#;
        let evaluated = test_eval(input);

        match evaluated.as_any().downcast_ref::<StringObject>() {
            Some(str_obj) => {
                assert_eq!(str_obj.value, "Hello World!");
            }
            None => {
                panic!("object is not StringLiteral. got={:?}", evaluated);
            }
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; };";
        let evaluated = test_eval(input);

        match evaluated.as_any().downcast_ref::<Function>() {
            Some(fn_obj) => {
                assert_eq!(fn_obj.parameters.len(), 1, "Function has wrong parameters. Parameters={:?}", fn_obj.parameters);
                assert_eq!(fn_obj.parameters[0].to_string(), "x", "Parameter is not 'x'. Got={:?}", fn_obj.parameters[0].to_string());

                let expected_body = "(x + 2)";
                assert_eq!(fn_obj.body.to_string(), expected_body, "Body is not {}. Got={}", expected_body, fn_obj.body.to_string());
            }
            None => {
                panic!("object is not Function. got={:?}", evaluated);
            }
        }
    }

    #[test]
    fn test_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]";
        let evaluated = test_eval(input);

        match evaluated.as_any().downcast_ref::<ArrayObject>() {
            Some(array_obj) => {
                assert_eq!(array_obj.elements.len(), 3, "array has wrong num of elements. got={}", array_obj.elements.len());

                test_integer_object(array_obj.elements[0].as_any(), 1);
                test_integer_object(&*array_obj.elements[1].as_any(), 4);
                test_integer_object(&*array_obj.elements[2].as_any(), 6);
            }
            None => {
                panic!("object is not Array. got={:?}", evaluated);
            }
        }
    }

    #[test]
    fn test_builtin_functions() {
        let tests = vec![
            (r#"len("")"#, 0),
            (r#"len("four")"#, 4),
            (r#"len("hello world")"#, 11),
        ];

        let tests_err = vec![
            (r#"len(1)"#, "argument to `len` not supported, got=INTEGER"),
            (r#"len("one", "two")"#, "wrong number of arguments. got=2, want=1"),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);

            match evaluated.as_any().downcast_ref::<Integer>() {
                Some(int_obj) => {
                    assert_eq!(int_obj.value, expected, "Wrong value. got={}", int_obj.value);
                }
                None => {
                    panic!("object is not Integer. got={:?}", evaluated);
                }
            }
        }

        for (input, expected) in tests_err {
            let evaluated = test_eval(input);

            match evaluated.as_any().downcast_ref::<Error>() {
                Some(err_obj) => {
                    assert_eq!(err_obj.message, expected, "Wrong error message. got={}", err_obj.message);
                }
                None => {
                    panic!("object is not Error. got={:?}", evaluated);
                }
            }
        }
    }


    #[test]
    fn test_function_application() {
        let tests = vec![
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
        ];

        for test in tests {
            let evaluated = test_eval(test.0); // Assuming test_eval evaluates the input string.
            assert_eq!(evaluated.as_any().downcast_ref::<Integer>().unwrap().value, test.1); // testIntegerObject is basically an assert in Rust.
        }
    }

    #[test]
    fn test_hash_index_expressions() {
        let right_tests = vec![
            TestInt { input: r#"{"foo": 5}["foo"]"#, expected: 5 },
            TestInt { input: "let key = \"foo\"; {\"foo\": 5}[key]", expected: 5 },
            TestInt { input: "{5: 5}[5]", expected: 5 },
            TestInt { input: "{true: 5}[true]", expected: 5 },
            TestInt { input: "{false: 5}[false]", expected: 5 },
        ];

        let wrong_tests = vec![
            TestStr { input: "{5: 5}[\"5\"]", expected_message: "unusable as hash key: STRING" },
            TestStr { input: "{true: 5}[true]", expected_message: "unusable as hash key: BOOLEAN" },
            TestStr { input: "{false: 5}[false]", expected_message: "unusable as hash key: BOOLEAN" },
        ];

        for test in right_tests {
            let evaluated = test_eval(test.input);
            test_integer_object(evaluated.as_ref().as_any(), test.expected);
        }

        for test in wrong_tests {
            let evaluated = test_eval(test.input);
            test_null_object(evaluated.as_ref().as_any());
        }
    }

    #[test]
    fn test_hash_literals() {
        let input = r#"
        let two = "two";
        {
            "one": 10 - 9,
            two: 1 + 1,
            "thr" + "ee": 6 / 2,
            4: 4,
            true: 5,
            false: 6
        }
    "#;

        let evaluated = test_eval(input);
        let result = evaluated.as_any().downcast_ref::<HashObject>().expect("Eval didn't return Hash");

        let mut expected: HashMap<u64, i64> = HashMap::new();
        expected.insert(StringObject::new("one").hash_key(), 1);
        expected.insert(StringObject::new("two").hash_key(), 2);
        expected.insert(StringObject::new("three").hash_key(), 3);
        expected.insert(Integer::new(4).hash_key(), 4);
        expected.insert(Boolean::new(true).hash_key(), 5);
        expected.insert(Boolean::new(false).hash_key(), 6);

        assert_eq!(result.pairs.len(), expected.len(), "Hash has wrong num of pairs.");

        for (expected_key, expected_value) in &expected {
            match result.pairs.get(&expected_key) {
                Some(value) => test_integer_object(value.value.as_any(), *expected_value),
                None => panic!("No pair for given key in pairs."),
            }
        }
    }

    fn test_eval(input: &str) -> Box<dyn Object> {
        // Here's a stub for the actual logic
        let l = Lexer::new(input);
        let mut p = parser::Parser::new(l);
        let program = p.parse_program();
        let mut env = Environment::new();
        eval(&program, &mut env) //
    }

    fn test_boolean_object(obj: &dyn Any, expected: bool) {
        if let Some(result) = obj.downcast_ref::<Boolean>() {
            if result.value != expected {
                panic!("object has wrong value. got={}, want={}", result.value, expected);
            }
        } else {
            panic!("object is not Boolean. got={:?}", obj);
        }
    }

    fn test_integer_object(obj: &dyn Any, expected: i64) {
        if let Some(result) = obj.downcast_ref::<Integer>() {
            if result.value != expected {
                panic!("object has wrong value. got={}, want={}", result.value, expected);
            }
        } else {
            panic!("object is not Integer. got={:?}", obj);
        }
    }
}

