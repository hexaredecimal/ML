use super::{identifier, sp};
use crate::ir::raw::*;
use crate::ir::*;
use crate::parser::types::type_literal;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_until, take_while1};
use nom::combinator::opt;
use nom::error::{context, VerboseError};
use nom::multi::{many0, many1, separated_list0};
use nom::sequence::tuple;
use nom::IResult;

fn int_literal(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (num, ty)) = tuple((
        take_while1(move |c: char| c.is_numeric()),
        many0(tuple((sp, tag("of"), sp, type_literal))),
    ))(i)?;

    let ty = if ty.is_empty() {
        Type::Int
    } else {
        let c = ty.first().unwrap();
        let (_, _, _, ty) = c;
        ty.clone()
    };

    Ok((
        i,
        RawNode::new(RawExpression::Integer(num.parse().unwrap(), Box::new(ty))),
    ))
}

fn float_literal(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (u, _, f, conv)) = tuple((
        take_while1(move |c: char| c.is_numeric()),
        tag("."),
        take_while1(move |c: char| c.is_numeric()),
        many0(tuple((sp, tag("of"), sp, type_literal))),
    ))(i)?;

    let ty = if conv.is_empty() {
        Type::Double
    } else {
        let c = conv.first().unwrap();
        let (_, _, _, ty) = c;
        ty.clone()
    };

    Ok((
        i,
        RawNode::new(RawExpression::Decimal(
            format!("{}.{}", u, f).parse().unwrap(),
            Box::new(ty.clone()),
        )),
    ))
}

fn char_literal(_i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (_, c, _)) = tuple((tag("'"), take_while1(move |c: char| c != '\''), tag("'")))(_i)?;

    if c.len() > 1 {
        println!("Literal of `Char` type has more than one rune.\nNote: error happened here on this value `{}`", c);
        std::process::exit(1);
    }
    Ok((i, RawNode::new(RawExpression::Char(c.as_bytes()[0]))))
}

fn bool_literal(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, val) = alt((tag("true"), tag("false")))(i)?;
    Ok((i, RawNode::new(RawExpression::Bool(val == "true"))))
}

fn array_literal(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (_, _, exprs, _, _)) = context(
        "Array literal",
        tuple((
            tag("["),
            sp,
            separated_list0(tuple((sp, tag(","), sp)), expression),
            sp,
            tag("]"),
        )),
    )(i)?;
    Ok((i, RawNode::new(RawExpression::Array(exprs.to_vec()))))
}

fn null_of(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (_, t)) = tuple((tag("null"), many0(tuple((sp, tag("of"), sp, type_literal)))))(i)?;

    let ty = if t.is_empty() {
        Box::new(Type::Any)
    } else {
        let (_, _, _, last) = t.first().unwrap();
        Box::new(last.clone())
    };

    Ok((i, RawNode::new(RawExpression::Null(ty))))
}

fn unit_literal(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, _) = tuple((tag("()"),))(i)?;

    // let t = t.0;
    Ok((i, RawNode::new(RawExpression::Unit)))
}

fn str_literal(_i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (_, c, _)) = tuple((tag("\""), take_until("\""), tag("\"")))(_i)?;

    let c = c.to_string();
    let c = c.replace('\n', "\\n");
    Ok((i, RawNode::new(RawExpression::String(c.to_string()))))
}

fn literal(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    alt((
        str_literal,
        unit_literal,
        null_of,
        float_literal,
        int_literal,
        char_literal,
        bool_literal,
        array_literal,
    ))(i)
}

fn parens(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (_, _, expr, _, _)) = tuple((tag("("), sp, expression_, sp, tag(")")))(i)?;
    Ok((i, expr))
}

pub fn fun_call(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    // TODO: Fix the parser for function call such that it is generic for all
    //       kinds of functions. For now function calls are parsed as
    //       <ID> (<ARGS>)
    //       What I want to parse is
    //       <EXPR> (<ARGS>)
    //       Currenty the parser has an issue parsing this.
    let (i, (id, _, _, _, args, _, _)) = tuple((
        identifier,
        sp,
        tag("("),
        sp,
        separated_list0(tuple((sp, tag(","), sp)), expression),
        sp,
        tag(")"),
    ))(i)?;

    Ok((
        i,
        RawNode::new(RawExpression::FunCall(id.to_string(), args.to_vec())),
    ))
}

pub fn lambda_call(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    // TODO: Fix the parser for function call such that it is generic for all
    //       kinds of functions. For now function calls are parsed as
    //       <ID> (<ARGS>)
    //       What I want to parse is
    //       <EXPR> (<ARGS>)
    //       Currenty the parser has an issue parsing this.
    let (i, (_, _, id, _, _, args, _, _)) = tuple((
        tag("["),
        sp,
        expression,
        sp,
        tag(":"),
        separated_list0(tuple((sp, tag(","), sp)), expression),
        sp,
        tag("]"),
    ))(i)?;

    let _ = i; // TODO: Remove this. This is done to suppress a warning
    println!("expr: {:?} -> args: {:?}", id, args);
    todo!()
}

pub fn identifier_expr(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, id) = identifier(i)?;
    Ok((i, RawNode::new(RawExpression::Id(id.to_string()))))
}

fn terminal(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    alt((parens, literal, fun_call, identifier_expr, lambda_call))(i)
}

fn deref_expr(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (ops, expr)) = tuple((many0(tuple((alt((tag("-"), tag("!"))), sp))), terminal))(i)?;
    Ok((
        i,
        ops.into_iter().rev().fold(expr, |prev, (op, _)| match op {
            "-" => RawNode::new(RawExpression::UnaryOp(UnaryOp::Minus, Box::new(prev))),
            "!" => RawNode::new(RawExpression::UnaryOp(UnaryOp::Not, Box::new(prev))),
            _ => unreachable!(),
        }),
    ))
}

fn factor(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (first, remainder)) = tuple((
        deref_expr,
        many0(tuple((sp, tag(".["), sp, expression, sp, tag("]")))),
    ))(i)?;
    Ok((
        i,
        remainder
            .into_iter()
            .fold(first, |prev, (_, _, _, next, _, _)| {
                RawNode::new(RawExpression::BinaryOp(
                    BinaryOp::ArrayDeref,
                    Box::new(prev),
                    Box::new(next),
                ))
            }),
    ))
}

fn term(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (first, remainder)) = tuple((
        factor,
        many0(tuple((sp, alt((tag("*"), tag("/"), tag("%"))), sp, factor))),
    ))(i)?;
    Ok((
        i,
        remainder
            .into_iter()
            .fold(first, |prev, (_, op, _, next)| match op {
                "*" => RawNode::new(RawExpression::BinaryOp(
                    BinaryOp::Multiply,
                    Box::new(prev),
                    Box::new(next),
                )),
                "/" => RawNode::new(RawExpression::BinaryOp(
                    BinaryOp::Divide,
                    Box::new(prev),
                    Box::new(next),
                )),
                "%" => RawNode::new(RawExpression::BinaryOp(
                    BinaryOp::Mod,
                    Box::new(prev),
                    Box::new(next),
                )),
                _ => unreachable!(),
            }),
    ))
}

fn additive_expr(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (first, remainder)) = tuple((
        term,
        many0(tuple((sp, alt((tag("+"), tag("-"), tag("::"))), sp, term))),
    ))(i)?;
    Ok((
        i,
        remainder
            .into_iter()
            .fold(first, |prev, (_, op, _, next)| match op {
                "+" => RawNode::new(RawExpression::BinaryOp(
                    BinaryOp::Add,
                    Box::new(prev),
                    Box::new(next),
                )),
                "-" => RawNode::new(RawExpression::BinaryOp(
                    BinaryOp::Sub,
                    Box::new(prev),
                    Box::new(next),
                )),
                "::" => RawNode::new(RawExpression::FieldAccess(Box::new(prev), Box::new(next))),
                _ => unreachable!(),
            }),
    ))
}

fn relational_expr(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (first, remainder)) = tuple((
        additive_expr,
        many0(tuple((
            sp,
            alt((tag("<"), tag(">"))),
            opt(tag("=")),
            sp,
            term,
        ))),
    ))(i)?;
    Ok((
        i,
        remainder
            .into_iter()
            .fold(first, |prev, (_, op, eq, _, next)| match (op, eq) {
                ("<", None) => RawNode::new(RawExpression::BinaryOp(
                    BinaryOp::LessThan,
                    Box::new(prev),
                    Box::new(next),
                )),
                ("<", Some("=")) => RawNode::new(RawExpression::BinaryOp(
                    BinaryOp::LessThanOrEqual,
                    Box::new(prev),
                    Box::new(next),
                )),
                (">", None) => RawNode::new(RawExpression::BinaryOp(
                    BinaryOp::GreaterThan,
                    Box::new(prev),
                    Box::new(next),
                )),
                (">", Some("=")) => RawNode::new(RawExpression::BinaryOp(
                    BinaryOp::GreaterThanOrEqual,
                    Box::new(prev),
                    Box::new(next),
                )),
                _ => unreachable!(),
            }),
    ))
}

fn equality_expr(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (first, remainder)) = tuple((
        relational_expr,
        many0(tuple((
            sp,
            alt((tag("=="), tag("!="))),
            sp,
            relational_expr,
        ))),
    ))(i)?;
    Ok((
        i,
        remainder
            .into_iter()
            .fold(first, |prev, (_, op, _, next)| match op {
                "==" => RawNode::new(RawExpression::BinaryOp(
                    BinaryOp::Equal,
                    Box::new(prev),
                    Box::new(next),
                )),
                "!=" => RawNode::new(RawExpression::BinaryOp(
                    BinaryOp::NotEqual,
                    Box::new(prev),
                    Box::new(next),
                )),
                _ => unreachable!(),
            }),
    ))
}

fn logical_and_expr(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (first, remainder)) = tuple((
        equality_expr,
        many0(tuple((sp, tag("&&"), sp, equality_expr))),
    ))(i)?;
    Ok((
        i,
        remainder
            .into_iter()
            .fold(first, |prev, (_, _op, _, next)| {
                RawNode::new(RawExpression::BinaryOp(
                    BinaryOp::And,
                    Box::new(prev),
                    Box::new(next),
                ))
            }),
    ))
}

fn logical_or_expr(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (first, remainder)) = tuple((
        logical_and_expr,
        many0(tuple((sp, tag("||"), sp, logical_and_expr))),
    ))(i)?;
    Ok((
        i,
        remainder
            .into_iter()
            .fold(first, |prev, (_, _op, _, next)| {
                RawNode::new(RawExpression::BinaryOp(
                    BinaryOp::Or,
                    Box::new(prev),
                    Box::new(next),
                ))
            }),
    ))
}

fn conditional_expr(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (_, _, top_cond, _, _, _, top_exp, _, elifs, _, _, top_alt)) = tuple((
        tag("if"),
        sp,
        expression,
        sp,
        tag("then"),
        sp,
        expression,
        sp,
        many0(tuple((
            tag("else"),
            sp,
            tag("if"),
            sp,
            expression,
            sp,
            tag("then"),
            sp,
            expression,
            sp,
        ))),
        tag("else"),
        sp,
        expression,
    ))(i)?;

    let mut node = top_alt;
    for (_, _, _, _, cond, _, _, _, expr, _) in elifs.into_iter().rev() {
        node = RawNode::new(RawExpression::Conditional(
            Box::new(cond),
            Box::new(expr),
            Box::new(node),
        ));
    }
    node = RawNode::new(RawExpression::Conditional(
        Box::new(top_cond),
        Box::new(top_exp),
        Box::new(node),
    ));

    Ok((i, node))
}

fn named_expression(i: &str) -> IResult<&str, (&str, RawNode), VerboseError<&str>> {
    let (i, (id, _, _, _, exp, _)) = tuple((identifier, sp, tag("="), sp, expression, sp))(i)?;
    Ok((i, (id, exp)))
}

fn let_expr(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (_, _, id, _, _, expr)) = tuple((
        tag("let"),
        sp,
        many1(named_expression),
        tag("in"),
        sp,
        expression,
    ))(i)?;

    let decls: Vec<RawExpression> = id
        .into_iter()
        .map(|f| {
            let (name, expr) = f;
            RawExpression::Let(name.to_string(), Box::new(expr))
        })
        .collect();

    Ok((
        i,
        RawNode::new(RawExpression::Lets(decls.clone(), Box::new(expr))),
    ))
}

fn val_destruct(i: &str) -> IResult<&str, TempExpr, VerboseError<&str>> {
    let (i, (_, identifiers, _)) = tuple((
        tag("("),
        separated_list0(tuple((sp, tag(","), sp)), identifier),
        tag(")"),
    ))(i)?;

    let identifiers: Vec<_> = identifiers.into_iter().map(|f| f.to_string()).collect();

    Ok((i, TempExpr::Ids(identifiers)))
}

fn val_id(i: &str) -> IResult<&str, TempExpr, VerboseError<&str>> {
    let (i, e) = tuple((identifier,))(i)?;
    let (e,) = e;
    Ok((i, TempExpr::Id(e.to_string())))
}

fn val_alt(i: &str) -> IResult<&str, TempExpr, VerboseError<&str>> {
    let (i, e) = alt((val_id, val_destruct))(i)?;
    Ok((i, e))
}

fn val_expr(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (_, _, ids, _, _, _, val)) =
        tuple((tag("val"), sp, val_alt, sp, tag("="), sp, expression))(i)?;

    match ids {
        TempExpr::Id(id) => Ok((
            i,
            RawNode::new(RawExpression::Val(id.to_string(), Box::new(val))),
        )),
        TempExpr::Ids(v) => Ok((
            i,
            RawNode::new(RawExpression::Destructure(v.clone(), Box::new(val))),
        )),
    }
}

fn block_expr(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (_, _, stmts, _, _)) =
        tuple((tag("{"), sp, separated_list0(sp, expression), sp, tag("}")))(i)?;

    Ok((i, RawNode::new(RawExpression::Block(stmts))))
}

fn cases(i: &str) -> IResult<&str, (RawNode, RawNode), VerboseError<&str>> {
    let (i, (a, _, _, _, e)) = tuple((expression, sp, tag("->"), sp, expression))(i)?;

    Ok((i, (a, e)))
}

fn match_expr(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (_, _, cond, _, _, _, casesd, _, _)) = tuple((
        tag("match"),
        sp,
        expression,
        sp,
        tag("{"),
        sp,
        separated_list0(sp, cases),
        sp,
        tag("}"),
    ))(i)?;

    Ok((
        i,
        RawNode::new(RawExpression::Match(Box::new(cond), casesd)),
    ))
}

fn abs_expr(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (_, _, e, _, _)) = tuple((tag("|"), sp, expression, sp, tag("|")))(i)?;

    Ok((i, RawNode::new(RawExpression::Abs(Box::new(e)))))
}
fn embed_expr(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (_, _, _, _, e, _, _)) = tuple((
        tag("java"),
        sp,
        tag("{"),
        sp,
        separated_list0(sp, str_literal),
        sp,
        tag("}"),
    ))(i)?;

    let s: Vec<_> = e
        .into_iter()
        .map(|f| match f.expr() {
            RawExpression::String(x) => x.clone(),
            _ => unreachable!(),
        })
        .collect();

    let s = s.join("\n");
    let e = RawExpression::String(s);
    let e = RawNode::new(e);
    Ok((i, RawNode::new(RawExpression::Embed(Box::new(e)))))
}

fn normargument(i: &str) -> IResult<&str, (String, RawNode), VerboseError<&str>> {
    let (i, (name, _, _, _, arg_type)) = tuple((identifier, sp, tag(":"), sp, expression))(i)?;
    Ok((i, (name.to_string(), arg_type)))
}

pub fn record_expression(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (name, _, _, _, decls, _, _)) = tuple((
        identifier,
        sp,
        tag("{"),
        sp,
        separated_list0(tuple((sp, tag(","), sp)), normargument),
        sp,
        tag("}"),
    ))(i)?;

    Ok((
        i,
        RawNode::new(RawExpression::RecordLiteral(
            name.to_string(),
            decls.clone(),
        )),
    ))
}

pub fn enum_node(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, a) = alt((fun_call, identifier_expr))(i)?;
    Ok((i, a))
}

fn normargument_typed(i: &str) -> IResult<&str, (String, Type), VerboseError<&str>> {
    let (i, (name, _, _, _, arg_type)) = tuple((identifier, sp, tag(":"), sp, type_literal))(i)?;
    Ok((i, (name.to_string(), arg_type)))
}

pub fn lambda_expression(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (_, _, _, args, _, _, _, _, ret, _, _, _, body)) = tuple((
        tag("fn"),
        sp,
        tag("("),
        separated_list0(tuple((sp, tag(","), sp)), normargument_typed),
        tag(")"),
        sp,
        tag(":"),
        sp,
        type_literal,
        sp,
        tag("=>"),
        sp,
        expression,
    ))(i)?;

    Ok((
        i,
        RawNode::new(RawExpression::Lambda(
            args.clone(),
            Box::new(ret),
            Box::new(body),
        )),
    ))
}

pub fn expression_(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    alt((
        lambda_expression,
        record_expression,
        embed_expr,
        abs_expr,
        match_expr,
        let_expr,
        block_expr,
        val_expr,
        conditional_expr,
        logical_or_expr,
    ))(i)
}

pub fn expression_null_check(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (e, a)) = tuple((
        expression_,
        many0(tuple((sp, tag("?"), many0(type_literal)))),
    ))(i)?;

    if a.is_empty() {
        return Ok((i, e));
    }

    let (_, _, t) = a.last().unwrap();
    let e = if t.is_empty() {
        RawExpression::SimpleNullCheck(Box::new(e))
    } else {
        let ty = t.last().unwrap();
        let ty = ty.clone();
        RawExpression::NullCheck(Box::new(e), Box::new(ty))
    };

    Ok((i, RawNode::new(e)))
}

pub fn cast_expression(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (e, a)) = tuple((
        expression_null_check,
        many0(tuple((sp, tag("as"), sp, type_literal))),
    ))(i)?;

    if a.is_empty() {
        return Ok((i, e));
    }

    let (_, _, _, t_) = a.last().unwrap();
    Ok((
        i,
        RawNode::new(RawExpression::Cast(Box::new(e), Box::new(t_.clone()))),
    ))
}

pub fn expression(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (e, a)) = tuple((
        cast_expression,
        many0(tuple((sp, tag("."), sp, cast_expression))),
    ))(i)?;

    if a.is_empty() {
        return Ok((i, e));
    }

    //let (_, _, _, t_) = a.last().unwrap();

    let mut result: Option<RawNode> = None;
    for (i, (_, _, _, expr)) in a.into_iter().enumerate() {
        if result.is_none() {
            // The first one
            result = Some(RawNode::new(RawExpression::DotExpression(
                Box::new(e.clone()),
                Box::new(expr.clone()),
            )));
        } else {
            let last = result.unwrap();
            result = Some(RawNode::new(RawExpression::DotExpression(
                Box::new(last.clone()),
                Box::new(expr.clone()),
            )));
        }
    }

    let result = result.unwrap();

    Ok((i, result))
}

#[test]
fn let_test() {
    let left = RawNode::new(RawExpression::Lets(
        vec![
            RawExpression::Let(
                "num".to_string(),
                Box::new(RawNode::new(RawExpression::Integer(
                    10,
                    Box::new(Type::Int),
                ))),
            ),
            RawExpression::Let(
                "age".to_string(),
                Box::new(RawNode::new(RawExpression::Id("num".to_string()))),
            ),
        ],
        Box::new(RawNode::new(RawExpression::Id("num".to_string()))),
    ));

    assert_eq!(let_expr("let num = 10 age = num in num"), Ok(("", left)));
}

#[test]
fn val_test() {
    let left = RawNode::new(RawExpression::Val(
        "foo".to_string(),
        Box::new(RawNode::new(RawExpression::Bool(false))),
    ));

    assert_eq!(val_expr("val foo = false"), Ok(("", left)));
}

#[test]
fn logical_and_test() {
    let left = RawNode::new(RawExpression::BinaryOp(
        BinaryOp::And,
        Box::new(RawNode::new(RawExpression::Bool(true))),
        Box::new(RawNode::new(RawExpression::Bool(false))),
    ));

    assert_eq!(logical_or_expr("true && false"), Ok(("", left)));
}

#[test]
fn logical_or_test() {
    let left = RawNode::new(RawExpression::BinaryOp(
        BinaryOp::Or,
        Box::new(RawNode::new(RawExpression::Bool(true))),
        Box::new(RawNode::new(RawExpression::Bool(false))),
    ));

    assert_eq!(logical_or_expr("true || false"), Ok(("", left)));
}

#[test]
fn abs_test() {
    let left = RawNode::new(RawExpression::Abs(Box::new(RawNode::new(
        RawExpression::Integer(100, Box::new(Type::Int)),
    ))));

    assert_eq!(abs_expr("| 100 |"), Ok(("", left)));
}

#[test]
fn embed_test() {
    let left = RawNode::new(RawExpression::Embed(Box::new(RawNode::new(
        RawExpression::String("System.out.println()".to_string()),
    ))));
    assert_eq!(
        embed_expr("java { \"System.out.println()\" }"),
        Ok(("", left))
    );
}

#[test]
fn block_test() {
    let left = RawNode::new(RawExpression::Block(vec![RawNode::new(RawExpression::Id(
        "bar".to_string(),
    ))]));
    assert_eq!(block_expr("{ bar }"), Ok(("", left)));
}

#[test]
fn empty_block_test() {
    let left = RawNode::new(RawExpression::Block(vec![]));
    assert_eq!(block_expr("{}"), Ok(("", left)));
}

#[test]
fn simple_null_check_test() {
    let left = RawNode::new(RawExpression::Id("Foo".to_string()));
    assert_eq!(
        expression_null_check("Foo?"),
        Ok((
            "",
            RawNode::new(RawExpression::SimpleNullCheck(Box::new(left)))
        ))
    );
}

#[test]
fn null_check_test() {
    let left = RawNode::new(RawExpression::Id("Foo".to_string()));
    assert_eq!(
        expression_null_check("Foo?String"),
        Ok((
            "",
            RawNode::new(RawExpression::NullCheck(
                Box::new(left),
                Box::new(Type::String)
            ))
        ))
    );
}

#[test]
fn record_literal_test() {
    let mut args: Vec<(String, RawNode)> = vec![];
    args.push((
        "bar".to_string(),
        RawNode::new(RawExpression::Integer(10, Box::new(Type::Int))),
    ));
    assert_eq!(
        record_expression("Foo {bar: 10}"),
        Ok((
            "",
            RawNode::new(RawExpression::RecordLiteral("Foo".to_string(), args))
        ))
    );
}

#[test]
fn enum_literal_test() {
    let args = RawNode::new(RawExpression::Id("Bar".to_string()));
    assert_eq!(
        enum_expression("Foo.Bar"),
        Ok((
            "",
            RawNode::new(RawExpression::EnumLiteral(
                "Foo".to_string(),
                Box::new(args)
            ))
        ))
    );
}

#[test]
fn variant_enum_literal_test() {
    let args = RawNode::new(RawExpression::FunCall(
        "Bar".to_string(),
        vec![RawNode::new(RawExpression::Integer(0, Box::new(Type::Int)))],
    ));
    assert_eq!(
        enum_expression("Foo.Bar(0)"),
        Ok((
            "",
            RawNode::new(RawExpression::EnumLiteral(
                "Foo".to_string(),
                Box::new(args)
            ))
        ))
    );
}

#[test]
fn expression_test() {
    assert_eq!(
        relational_expr("1 < 2"),
        Ok((
            "",
            RawNode::new(RawExpression::BinaryOp(
                BinaryOp::LessThan,
                Box::new(RawNode::new(RawExpression::Integer(1, Box::new(Type::Int)))),
                Box::new(RawNode::new(RawExpression::Integer(2, Box::new(Type::Int))))
            ))
        ))
    );
    assert_eq!(
        relational_expr("1 <= 2"),
        Ok((
            "",
            RawNode::new(RawExpression::BinaryOp(
                BinaryOp::LessThanOrEqual,
                Box::new(RawNode::new(RawExpression::Integer(1, Box::new(Type::Int)))),
                Box::new(RawNode::new(RawExpression::Integer(2, Box::new(Type::Int))))
            ))
        ))
    );
    assert_eq!(
        relational_expr("1 + 1 <= 2"),
        Ok((
            "",
            RawNode::new(RawExpression::BinaryOp(
                BinaryOp::LessThanOrEqual,
                Box::new(RawNode::new(RawExpression::BinaryOp(
                    BinaryOp::Add,
                    Box::new(RawNode::new(RawExpression::Integer(1, Box::new(Type::Int)))),
                    Box::new(RawNode::new(RawExpression::Integer(1, Box::new(Type::Int))))
                ))),
                Box::new(RawNode::new(RawExpression::Integer(2, Box::new(Type::Int))))
            ))
        ))
    );
    assert_eq!(
        relational_expr("1 + -1 <= 2"),
        Ok((
            "",
            RawNode::new(RawExpression::BinaryOp(
                BinaryOp::LessThanOrEqual,
                Box::new(RawNode::new(RawExpression::BinaryOp(
                    BinaryOp::Add,
                    Box::new(RawNode::new(RawExpression::Integer(1, Box::new(Type::Int)))),
                    Box::new(RawNode::new(RawExpression::UnaryOp(
                        UnaryOp::Minus,
                        Box::new(RawNode::new(RawExpression::Integer(1, Box::new(Type::Int))))
                    )))
                ))),
                Box::new(RawNode::new(RawExpression::Integer(2, Box::new(Type::Int))))
            ))
        ))
    );
}
