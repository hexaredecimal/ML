use super::{identifier, sp};
use crate::ir::Type;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while1};
use nom::multi::separated_list0;
use nom::error::VerboseError;
use nom::sequence::tuple;
use nom::IResult;

fn array_type(i: &str) -> IResult<&str, Type, VerboseError<&str>> {
    let (i, (_, _, elem, _, _, _, len, _, _)) = tuple((
        tag("["),
        sp,
        type_literal,
        sp,
        tag(";"),
        sp,
        take_while1(move |c: char| c.is_numeric()),
        sp,
        tag("]"),
    ))(i)?;
    Ok((i, Type::Array(len.parse().unwrap(), Box::new(elem))))
}

fn list_type(i: &str) -> IResult<&str, Type, VerboseError<&str>> {
    let (i, (_, _, _, _, t, _, _)) =
        tuple((tag("List"), sp, tag("["), sp, type_literal, sp, tag("]")))(i)?;

    Ok((i, Type::List(Box::new(t))))
}

fn bool_type(i: &str) -> IResult<&str, Type, VerboseError<&str>> {
    let (i, _) = tag("Bool")(i)?;
    Ok((i, Type::Bool))
}

fn char_type(i: &str) -> IResult<&str, Type, VerboseError<&str>> {
    let (i, _) = tag("Char")(i)?;
    Ok((i, Type::Char))
}

fn int_type(i: &str) -> IResult<&str, Type, VerboseError<&str>> {
    let (i, _) = tag("Int")(i)?;
    Ok((i, Type::Int))
}

fn int32_type(i: &str) -> IResult<&str, Type, VerboseError<&str>> {
    let (i, _) = tag("Int32")(i)?;
    Ok((i, Type::Int32))
}

fn int64_type(i: &str) -> IResult<&str, Type, VerboseError<&str>> {
    let (i, _) = tag("Int64")(i)?;
    Ok((i, Type::Int64))
}

fn int8_type(i: &str) -> IResult<&str, Type, VerboseError<&str>> {
    let (i, _) = tag("Int8")(i)?;
    Ok((i, Type::Int8))
}

fn int16_type(i: &str) -> IResult<&str, Type, VerboseError<&str>> {
    let (i, _) = tag("Int16")(i)?;
    Ok((i, Type::Int16))
}

fn int128_type(i: &str) -> IResult<&str, Type, VerboseError<&str>> {
    let (i, _) = tag("Int128")(i)?;
    Ok((i, Type::Int128))
}

fn int_types(i: &str) -> IResult<&str, Type, VerboseError<&str>> {
    alt((
        int32_type,
        int64_type,
        int8_type,
        int16_type,
        int128_type,
        int_type,
    ))(i)
}

fn float_type(i: &str) -> IResult<&str, Type, VerboseError<&str>> {
    let (i, _) = tag("Float")(i)?;
    Ok((i, Type::Float))
}

fn double_type(i: &str) -> IResult<&str, Type, VerboseError<&str>> {
    let (i, _) = tag("Double")(i)?;
    Ok((i, Type::Double))
}

fn usz_type(i: &str) -> IResult<&str, Type, VerboseError<&str>> {
    let (i, _) = tag("Usize")(i)?;
    Ok((i, Type::Usize))
}

fn unit_type(i: &str) -> IResult<&str, Type, VerboseError<&str>> {
    let (i, _) = tag("Unit")(i)?;
    Ok((i, Type::Unit))
}

fn str_type(i: &str) -> IResult<&str, Type, VerboseError<&str>> {
    let (i, _) = tag("String")(i)?;
    Ok((i, Type::String))
}

fn any_type(i: &str) -> IResult<&str, Type, VerboseError<&str>> {
    let (i, _) = tag("Any")(i)?;
    Ok((i, Type::Any))
}

fn user_type(i: &str) -> IResult<&str, Type, VerboseError<&str>> {
    let (i, t) = tuple((identifier,))(i)?;
    let (t,) = t;

    Ok((i, Type::YourType(t.to_string())))
}

fn enum_type(i: &str) -> IResult<&str, Type, VerboseError<&str>> {
    let (i, (parent, _, _, _, child)) = tuple((
        identifier,
        sp, 
        tag("."), 
        sp, 
        identifier))(i)?;

    Ok((i, Type::EnumType(parent.to_string(), child.to_string())))
}

pub fn lambda(i: &str) -> IResult<&str, Type, VerboseError<&str>> {
    let (i, (_, _, _, args, _, _, _, _, ret)) = tuple((
        tag("fn"),
        sp,
        tag("("),
        separated_list0(tuple((sp, tag(","), sp)), type_literal),
        tag(")"),
        sp,
        tag(":"),
        sp,
        type_literal
    ))(i)?;

    Ok((i, Type::Lambda(Box::new(ret), args.clone())))
}


pub fn type_literal(i: &str) -> IResult<&str, Type, VerboseError<&str>> {
    alt((
        lambda,
        enum_type,
        any_type,
        str_type,
        unit_type,
        usz_type,
        double_type,
        list_type,
        char_type,
        float_type,
        int_types,
        bool_type,
        array_type,
        user_type,
    ))(i)
}
