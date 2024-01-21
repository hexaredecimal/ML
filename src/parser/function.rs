use super::expression::expression;
use super::types::type_literal;
use super::{identifier, sp};
use crate::ir::raw::RawFunction;
use crate::ir::Type;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::error::VerboseError;
use nom::multi::separated_list0;
use nom::sequence::tuple;
use nom::IResult;

fn normargument(i: &str) -> IResult<&str, (String, Type), VerboseError<&str>> {
    let (i, (name, _, _, _, arg_type)) = tuple((identifier, sp, tag(":"), sp, type_literal))(i)?;
    Ok((i, (name.to_string(), arg_type)))
}


fn vargument(i: &str) -> IResult<&str, (String, Type), VerboseError<&str>> {
    let (i, _) = tuple((
        tag("..."),
    ))(i)?; 

    Ok((i, ("".to_string(), Type::VarArgs)))
}


fn argument(i: &str) -> IResult<&str, (String, Type), VerboseError<&str>> {
    let (i, b) = alt((normargument, vargument))(i)?;

    Ok((i, b))
}


pub fn function(i: &str) -> IResult<&str, RawFunction, VerboseError<&str>> {
    let (i, (_, _, name, _, _, args, _, _, _, _, return_type, _, _, _, root)) = tuple((
        tag("fun"),
        sp,
        identifier,
        sp,
        tag("("),
        separated_list0(tuple((sp, tag(","), sp)), argument),
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

    let args_types = args.to_vec();
    Ok((
        i,
        RawFunction {
            name: name.to_string(),
            root,
            ty: Type::Function(Box::new(return_type), args_types),
            args,
        },
    ))
}
