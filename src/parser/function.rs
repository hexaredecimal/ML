use super::expression::*;
use super::types::type_literal;
use super::{identifier, sp};
use crate::ir::raw::*; 
use crate::ir::Type;
use nom::branch::alt;
use nom::error::VerboseError;
use nom::multi::separated_list0;
use nom::sequence::tuple;
use nom::IResult;
use nom::bytes::complete::{tag, take_until};


fn normargument(i: &str) -> IResult<&str, (String, Type), VerboseError<&str>> {
    let (i, (name, _, _, _, arg_type)) = tuple((identifier, sp, tag(":"), sp, type_literal))(i)?;
    Ok((i, (name.to_string(), arg_type)))
}

fn vargument(i: &str) -> IResult<&str, (String, Type), VerboseError<&str>> {
    let (i, _) = tuple((tag("..."),))(i)?;

    Ok((i, ("".to_string(), Type::VarArgs)))
}

fn argument(i: &str) -> IResult<&str, (String, Type), VerboseError<&str>> {
    let (i, b) = alt((normargument, vargument))(i)?;

    Ok((i, b))
}

pub fn record_type(i: &str) -> IResult<&str, TopLevel, VerboseError<&str>> {
    let (i, (_, _, name, _, _, args, _)) = tuple((
        tag("struct"),
        sp,
        identifier,
        sp,
        tag("("),
        separated_list0(tuple((sp, tag(","), sp)), normargument),
        tag(")"),
    ))(i)?;

    Ok((
        i,
        TopLevel::RecordType {
            name: name.to_string(),
            fields: args,
        },
    ))
}

pub fn cool_enm(i: &str) -> IResult<&str, EnumField, VerboseError<&str>> {
    let (i, (id, _, _, _, args, _, _)) = tuple((
        identifier,
        sp,
        tag("("),
        sp,
        separated_list0(tuple((sp, tag(","), sp)), normargument),
        sp,
        tag(")"),
    ))(i)?;
    let field = RecordType {name: id.to_string(), fields: args.clone()}; 

    Ok((
        i,EnumField::Rec(field)
    ))
}


pub fn norm_enm(i: &str) -> IResult<&str, EnumField, VerboseError<&str>> {
    let (i, (id,)) = tuple((identifier,))(i)?; 

    Ok((i, EnumField::Id(id.to_string())))
}

pub fn enum_arg(i: &str) -> IResult<&str, EnumField, VerboseError<&str>> {
    let (i, a) = alt((cool_enm, norm_enm))(i)?; 
    Ok((i, a))
}


pub fn enum_args(i: &str) -> IResult<&str, Vec<EnumField>, VerboseError<&str>> {
    let (i, (_, args)) = tuple((
        sp, 
        separated_list0(tuple((sp, tag("|"), sp)), enum_arg),
    ))(i)?; 
    Ok((i, args))
}

pub fn enum_type(i: &str) -> IResult<&str, TopLevel, VerboseError<&str>> {
    let (i, (_, _, name, _, _, _, args)) = tuple((
        tag("enum"),
        sp,
        identifier,
        sp,
        tag("="),
        sp, 
        enum_args,
    ))(i)?;

    let t = TopLevel::EnumType { name: name.to_string(), fields: args }; 
    Ok((i, t))
}



pub fn function(i: &str) -> IResult<&str, TopLevel, VerboseError<&str>> {
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
        TopLevel::RawFunction {
            name: name.to_string(),
            root,
            ty: Type::Function(Box::new(return_type), args_types),
            args,
        },
    ))
}

pub fn import_statement(i: &str) -> IResult<&str, TopLevel, VerboseError<&str>> {
    let (i, (_ ,_ , path)) = tuple((
        tag("using"), 
        sp, 
        separated_list0(tuple((sp, tag("::"), sp)), identifier)
    ))(i)?; 

    let path = path.into_iter().map(|f| f.to_string()).collect();

    Ok((i , 
        TopLevel::Import { path }
    ))
}


pub fn str_literal(_i: &str) -> IResult<&str, String, VerboseError<&str>> {
    let (i, (_, c, _)) = tuple((tag("\""), take_until("\""), tag("\"")))(_i)?;

    let c = c.to_string(); 
    let c = c.replace("\n", "\\n"); 
    Ok((i, c.to_string()))
}

fn lift(i: &str) -> IResult<&str, Type, VerboseError<&str>> {
    let (i, j) = str_literal(i)?;
    Ok((i, 
        Type::TypeLift(j)
    ))
}

fn type_or_lift(i: &str) -> IResult<&str, Type, VerboseError<&str>> {
    alt((type_literal, lift))(i)
}

pub fn alias_of(i: &str) -> IResult<&str, TopLevel, VerboseError<&str>> {
    let (i, (_, _, name, _, _, _, ty)) = tuple((
        tag("type"),
        sp, 
        identifier,
        sp,
        tag("="), 
        sp, 
        type_or_lift
    ))(i)?;

    Ok((i, 
        TopLevel::Alias { name: name.to_string(), value: ty }
    ))
}

pub fn top_levels(i: &str) -> IResult<&str, TopLevel, VerboseError<&str>> {
    let (i, e) = alt((import_statement, record_type, function, enum_type, alias_of))(i)?;
    Ok((i, e))
}
