use super::{identifier, sp};
use crate::ir::raw::*;
use crate::ir::*;
use crate::ir::sem::SemExpression;
use crate::parser::types::type_literal;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::combinator::opt;
use nom::error::{VerboseError, VerboseErrorKind};
use nom::multi::{many0, separated_list0, many1};
use nom::sequence::tuple;
use nom::{IResult, Err};

fn int_literal(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (num, ty)) = tuple((
        take_while1(move |c: char| c.is_numeric()), 
        many0(tuple((sp, tag("of"), sp, type_literal)))))(i)?;

    let ty = if ty.len() == 0 {
        Type::Int
    } else {
        let c = ty.first().unwrap();
        let (_, _, _, ty) = c;
        ty.clone()
    }; 


    Ok((i, RawNode::new(
        RawExpression::Integer(
            num.parse().unwrap(),
            Box::new(ty)
        ))
    ))
}

fn float_literal(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (u, _, f, conv)) = tuple((
        take_while1(move |c: char| c.is_numeric()),
        tag("."),
        take_while1(move |c: char| c.is_numeric()),
        many0(tuple((sp, tag("of"), sp, type_literal)))
    ))(i)?;
 
    let ty = if conv.len() == 0 {
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
            Box::new(ty.clone())
        )),
    ))
}

fn char_literal(_i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (_, c, _)) = tuple((
        tag("'"), 
        take_while1(move |c: char| c != '\''),
        tag("'"), 
    ))(_i)?; 

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
    let (i, (_, _, exprs, _, _)) = tuple((
        tag("["),
        sp,
        separated_list0(tuple((sp, tag(","), sp)), expression),
        sp,
        tag("]"),
    ))(i)?;
    Ok((i, RawNode::new(RawExpression::Array(exprs.to_vec()))))
}

fn null_of(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (_, _, _, _, t)) = tuple((
        tag("null"), 
        sp, 
        tag("of"), 
        sp, 
        type_literal
    ))(i)?;

    Ok((i, RawNode::new(RawExpression::Null(Box::new(t)))))
}


fn unit_literal(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (t)) = tuple((
        tag("()"),
    ))(i)?;

    let t = t.0; 
    Ok((i, RawNode::new(RawExpression::Unit)))
}

fn str_literal(_i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (_, c, _)) = tuple((
        tag("\""), 
        take_while1(move |c: char| c != '\"'),
        tag("\""), 
    ))(_i)?; 

    Ok((i, RawNode::new(RawExpression::String(c.to_string()))))
}


fn literal(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    alt((str_literal, unit_literal, null_of, float_literal, int_literal, char_literal,  bool_literal, array_literal))(i)
}

fn parens(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (_, _, expr, _, _)) = tuple((tag("("), sp, expression_, sp, tag(")")))(i)?;
    Ok((i, expr))
}

fn fun_call(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
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



fn identifier_expr(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, id) = identifier(i)?;
    Ok((i, RawNode::new(RawExpression::Id(id.to_string()))))
}

fn terminal(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    alt((parens, literal, fun_call, identifier_expr))(i)
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
        many0(tuple((sp, tag(".["), sp, int_literal, sp, tag("]")))),
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
        many0(tuple((sp, alt((tag("*"), tag("/"))), sp, factor))),
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
                _ => unreachable!(),
            }),
    ))
}

fn additive_expr(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (first, remainder)) = tuple((
        term,
        many0(tuple((sp, alt((tag("+"), tag("-"))), sp, term))),
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
    let (i, (id, _, _,_, exp, _)) = tuple((identifier, sp, tag("="), sp, expression, sp))(i)?; 
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

    let decls: Vec<RawExpression> = id.into_iter().map(|f| {
        let (name, expr) = f; 
        RawExpression::Let(name.to_string(), Box::new(expr))
    }).collect(); 

    Ok((
        i,
        RawNode::new(RawExpression::Lets(
            decls.clone(), 
            Box::new(expr),
        )),
    ))
}


fn val_expr(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (_, _, id, _, _, _, val)) = tuple((
        tag("val"),
        sp,
        identifier,
        sp,
        tag("="),
        sp,
        expression,
    ))(i)?;
    Ok((
        i,
        RawNode::new(RawExpression::Val(
            id.to_string(),
            Box::new(val),
        )),
    ))
}

fn block_expr(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (_, _, stmts, _, _)) = tuple((
        tag("{"), 
        sp, 
        separated_list0(sp, expression),
        sp, 
        tag("}")
    ))(i)?;

    Ok((
        i,
        RawNode::new(RawExpression::Block(stmts))
    ))
}

fn cases(i: &str) -> IResult<&str, (RawNode, RawNode), VerboseError<&str>> {
    let (i, (a, _, _, _, e)) = tuple((
        expression, 
        sp, 
        tag("=>"),
        sp, 
        expression
    ))(i)?;

    Ok((i, 
        (a,e)))
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
        tag("}")
    ))(i)?;

    Ok((i,
        RawNode::new(
            RawExpression::Match(
                Box::new(cond), 
                casesd
            )
        )
    ))
}

fn abs_expr(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (_, _, e, _, _)) = tuple((
        tag("|"), 
        sp, 
        expression, 
        sp, 
        tag("|")
    ))(i)?;

    Ok((i, 
        RawNode::new(RawExpression::Abs(
            Box::new(e)
        ))
    ))
}
fn embed_expr(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (_, _, _, _, e, _, _)) = tuple((
        tag("java"), 
        sp, 
        tag("{"),
        sp,
        separated_list0(sp, str_literal),
        sp, 
        tag("}")
    ))(i)?;

    let s: Vec<_> = e.into_iter().map(|f| {
        match f.expr() {
            RawExpression::String(x) => x.clone(),
            _ => unreachable!()
        }
    }).collect();

    let s = s.join("\n");
    let e = RawExpression::String(s);
    let e = RawNode::new(e);  
    Ok((i, 
        RawNode::new(RawExpression::Embed(Box::new(e)))
    ))
}
 

pub fn expression_(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    alt((embed_expr, abs_expr, match_expr, let_expr, block_expr, val_expr, conditional_expr, logical_or_expr,))(i)
}

pub fn expression(i: &str) -> IResult<&str, RawNode, VerboseError<&str>> {
    let (i, (e, a)) = tuple((
        expression_,
        many0(
            tuple((
                sp, 
                tag("as"), 
                sp, 
                type_literal
            ))
        )
    ))(i)?;

    
    if a.len() == 0 {
        return Ok((i, e)); 
    }

    let (_, _, _, t_) = a.last().unwrap(); 
    Ok((i, 
        RawNode::new(RawExpression::Cast(
            Box::new(e), 
            Box::new(t_.clone())
        ))
    ))
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
