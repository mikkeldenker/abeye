mod db;
mod ts;

use camino::Utf8PathBuf;
pub use db::Database;
pub use ts::generate_ts;

use std::collections::{BTreeMap, HashSet};

use itertools::Itertools;
use utoipa::openapi as oapi;

#[salsa::jar(db = Db)]
pub struct Jar(
    InputApi,
    Type,
    Schema,
    generate_ts,
    schema_by_name,
    schema_ty,
    simplify_ty,
);

pub trait Db: salsa::DbWithJar<Jar> {}

impl<DB> Db for DB where DB: ?Sized + salsa::DbWithJar<Jar> {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Config {
    pub api_prefix: Option<Utf8PathBuf>,
}

#[salsa::input]
pub struct InputApi {
    #[return_ref]
    pub api: oapi::OpenApi,
    pub config: Config,
}

#[salsa::interned]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Type {
    kind: TypeKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum TypeKind {
    Reference(String),
    Object(BTreeMap<String, Property>),
    Array(Type),
    Tuple(Vec<Type>),
    Or(Vec<Type>),
    And(Vec<Type>),
    Number,
    Ident(String),
    String,
    Boolean,
    Null,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Property {
    ty: Type,
    optional: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RequestKind {
    Json(Type),
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ResponseKind {
    Plain,
    Json(Type),
    EventStream(Type),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Operation {
    path: String,
    query: BTreeMap<String, Type>,
    path_params: BTreeMap<String, Type>,
    body: Option<RequestKind>,
    response: Option<ResponseKind>,
}

#[salsa::tracked]
struct Schema {
    #[return_ref]
    schema: OapiSchema,
}

#[derive(Debug, Clone, PartialEq)]
struct OapiSchema {
    schema: oapi::Schema,
}

impl Eq for OapiSchema {}

impl Schema {
    fn from_oapi(db: &dyn crate::Db, schema: oapi::Schema) -> Schema {
        Schema::new(db, OapiSchema { schema })
    }
    fn inner(self, db: &dyn crate::Db) -> &oapi::Schema {
        &self.schema(db).schema
    }
}

impl Type {
    pub fn constants(self, db: &dyn crate::Db) -> Option<Vec<String>> {
        match self.kind(db) {
            TypeKind::Or(options)
                if options
                    .iter()
                    .all(|opt| matches!(opt.kind(db), TypeKind::Ident(_))) =>
            {
                Some(
                    options
                        .iter()
                        .map(|opt| match opt.kind(db) {
                            TypeKind::Ident(value) => value.clone(),
                            _ => unreachable!(),
                        })
                        .collect(),
                )
            }
            _ => None,
        }
    }
}

fn resolve_schema(db: &dyn crate::Db, api: InputApi, schema: &oapi::RefOr<oapi::Schema>) -> Schema {
    match schema {
        oapi::RefOr::Ref(reference) => {
            schema_by_name(db, api, reference.ref_location.clone()).unwrap()
        }
        oapi::RefOr::T(schema) => Schema::from_oapi(db, schema.clone()),
    }
}
fn resolve_schema_ty(
    db: &dyn crate::Db,
    api: InputApi,
    schema: &oapi::RefOr<oapi::Schema>,
) -> Type {
    schema_ty(db, api, resolve_schema(db, api, schema))
}
fn shallow_schema_ty(
    db: &dyn crate::Db,
    api: InputApi,
    schema: &oapi::RefOr<oapi::Schema>,
) -> Type {
    match schema {
        oapi::RefOr::Ref(reference) => {
            if let Some(name) = reference.ref_location.strip_prefix("#/components/schemas/") {
                if name.contains('_') {
                    resolve_schema_ty(db, api, schema)
                } else {
                    Type::new(db, TypeKind::Reference(name.to_string()))
                }
            } else {
                todo!()
            }
        }
        oapi::RefOr::T(schema) => schema_ty(db, api, Schema::from_oapi(db, schema.clone())),
    }
}

fn ty_by_name(db: &dyn crate::Db, api: InputApi, name: String) -> Type {
    shallow_schema_ty(
        db,
        api,
        &oapi::RefOr::Ref(oapi::schema::RefBuilder::new().ref_location(name).build()),
    )
}

fn operation(
    db: &dyn crate::Db,
    api: InputApi,
    path: String,
    operation: &oapi::path::Operation,
) -> Operation {
    let mut path_params = BTreeMap::new();
    let mut query = BTreeMap::new();

    for param in operation.parameters.iter().flat_map(|p| p.iter()) {
        match param.parameter_in {
            oapi::path::ParameterIn::Query => match &param.schema {
                Some(oapi::RefOr::Ref(_)) => todo!(),
                Some(oapi::RefOr::T(schema)) => {
                    let ty = shallow_schema_ty(db, api, &oapi::RefOr::T(schema.clone()));
                    query.insert(param.name.clone(), ty);
                }
                None => {}
            },
            oapi::path::ParameterIn::Path => {
                match &param.schema {
                    Some(oapi::RefOr::Ref(_)) => todo!(),
                    Some(oapi::RefOr::T(schema)) => {
                        let ty = shallow_schema_ty(db, api, &oapi::RefOr::T(schema.clone()));
                        path_params.insert(param.name.clone(), ty);
                    }
                    None => {}
                };
            }
            oapi::path::ParameterIn::Header => todo!(),
            oapi::path::ParameterIn::Cookie => todo!(),
        }
    }
    let body = if let Some(body) = &operation.request_body {
        assert_eq!(body.content.len(), 1, "only single content type supported");
        let (media_type, content) = body.content.iter().next().unwrap();
        let ty = match &content.schema {
            None => todo!(),
            Some(oapi::RefOr::Ref(reference)) => {
                ty_by_name(db, api, reference.ref_location.clone())
            }
            Some(oapi::RefOr::T(schema)) => simplify_ty(
                db,
                shallow_schema_ty(db, api, &oapi::RefOr::T(schema.clone())),
            ),
        };

        let ts = ty.ts(db);
        tracing::debug!(?media_type, ty=?ts, "request");
        match media_type.as_str() {
            "application/json" => Some(RequestKind::Json(ty)),
            _ => todo!("unhandled request media type: {media_type:?}"),
        }
    } else {
        None
    };

    if !path_params.is_empty() {
        for (path_param, ty) in &path_params {
            let ty = ty.ts(db);
            tracing::debug!(?path_param, ?ty);
        }
    }
    if !query.is_empty() {
        for (query_param, ty) in &query {
            let ty = ty.ts(db);
            tracing::debug!(?query_param, ?ty);
        }
    }

    let mut response = None;

    for (status, res) in &operation.responses.responses {
        response = match res {
            oapi::RefOr::Ref(_) => todo!(),
            oapi::RefOr::T(response) => {
                for (media_type, value) in &response.content {
                    if let Some(schema) = &value.schema {
                        let ty = simplify_ty(db, shallow_schema_ty(db, api, schema)).ts(db);
                        tracing::debug!(?status, ?media_type, ?ty, "response");
                    }
                }

                assert_eq!(response.content.len(), 1);

                let (media_type, value) = response.content.iter().next().unwrap();
                let ty = if let Some(schema) = &value.schema {
                    let ty = simplify_ty(db, shallow_schema_ty(db, api, schema));
                    let ts = ty.ts(db);
                    tracing::debug!(?status, ?media_type, ty=?ts, "response");
                    ty
                } else {
                    todo!()
                };
                match media_type.as_str() {
                    "text/plain" | "text/plain; charset=utf-8" => {
                        assert_eq!(ty, Type::new(db, TypeKind::String));
                        Some(ResponseKind::Plain)
                    }
                    "application/json" => Some(ResponseKind::Json(ty)),
                    "text/event-stream" => Some(ResponseKind::EventStream(ty)),
                    _ => todo!("unhandled request media type: {media_type:?}"),
                }
            }
        };
    }

    Operation {
        path,
        query,
        path_params,
        body,
        response,
    }
}

#[salsa::tracked]
fn schema_by_name(db: &dyn crate::Db, api: InputApi, name: String) -> Option<Schema> {
    tracing::debug!(?name, "schema_by_name");

    if let Some(name) = name.strip_prefix("#/components/schemas/") {
        schema_by_name(db, api, name.to_string())
    } else {
        match api.api(db).components.as_ref()?.schemas.get(&name)? {
            oapi::RefOr::Ref(reference) => {
                todo!("reference to: {reference:?}")
            }
            oapi::RefOr::T(schema) => Some(Schema::from_oapi(db, schema.clone())),
        }
    }
}

fn ty_ty(db: &dyn crate::Db, api: InputApi, ty: &oapi::Type, obj: &oapi::Object) -> Type {
    match ty {
        oapi::schema::Type::String => match &obj.enum_values {
            None => Type::new(db, TypeKind::String),
            Some(str) if str.is_empty() => Type::new(db, TypeKind::String),
            Some(str) => Type::new(
                db,
                TypeKind::Or(
                    str.iter()
                        .map(|e| match e {
                            serde_json::Value::String(s) => {
                                Type::new(db, TypeKind::Ident(s.clone()))
                            }
                            _ => unreachable!(),
                        })
                        .collect(),
                ),
            ),
        },

        oapi::schema::Type::Array => todo!(),

        oapi::Type::Null => Type::new(db, TypeKind::Null),

        oapi::Type::Number | oapi::Type::Integer => Type::new(db, TypeKind::Number),

        oapi::Type::Boolean => Type::new(db, TypeKind::Boolean),

        oapi::Type::Object => {
            let mut properties = BTreeMap::default();
            let required_fields = obj.required.clone();

            for (name, prop) in &obj.properties {
                let ty = shallow_schema_ty(db, api, &prop.clone());
                let required = required_fields.contains(&name.clone());
                properties.insert(
                    name.clone(),
                    Property {
                        ty,
                        optional: !required,
                    },
                );
            }

            Type::new(db, TypeKind::Object(properties))
        }
    }
}

#[salsa::tracked]
fn schema_ty(db: &dyn crate::Db, api: InputApi, schema: Schema) -> Type {
    match schema.inner(db) {
        oapi::Schema::Object(obj) => match &obj.schema_type {
            oapi::schema::SchemaType::Type(ty) => ty_ty(db, api, ty, &obj),
            oapi::schema::SchemaType::Array(array_ty) => {
                let types: HashSet<_> =
                    array_ty.iter().map(|ty| ty_ty(db, api, ty, &obj)).collect();

                if types.len() == 1 {
                    let ty = array_ty.first().unwrap();
                    Type::new(db, TypeKind::Array(ty_ty(db, api, ty, &obj)))
                } else if types.len() == 2 && types.contains(&Type::new(db, TypeKind::Null)) {
                    let non_null = types
                        .iter()
                        .find(|ty| !matches!(ty.kind(db), TypeKind::Null))
                        .unwrap();
                    simplify_ty(db, non_null.clone())
                } else {
                    Type::new(db, TypeKind::Tuple(types.into_iter().collect()))
                }
            }

            oapi::schema::SchemaType::AnyValue => todo!(),
        },
        oapi::Schema::OneOf(one_of) => Type::new(
            db,
            TypeKind::Or(
                one_of
                    .items
                    .iter()
                    .map(|item| shallow_schema_ty(db, api, item))
                    .collect(),
            ),
        ),
        oapi::Schema::AllOf(all_of) => Type::new(
            db,
            TypeKind::And(
                all_of
                    .items
                    .iter()
                    .map(|item| shallow_schema_ty(db, api, item))
                    .collect(),
            ),
        ),
        oapi::Schema::Array(array) => match &array.items {
            oapi::schema::ArrayItems::RefOrSchema(ref_or) => {
                Type::new(db, TypeKind::Array(shallow_schema_ty(db, api, &ref_or)))
            }
            oapi::schema::ArrayItems::False => Type::new(
                db,
                TypeKind::Tuple(
                    array
                        .prefix_items
                        .iter()
                        .map(|item| shallow_schema_ty(db, api, &oapi::RefOr::T(item.clone())))
                        .collect(),
                ),
            ),
        },
        oapi::Schema::AnyOf { .. } => todo!(),
        _ => todo!(),
    }
}

#[salsa::tracked]
fn simplify_ty(db: &dyn crate::Db, ty: Type) -> Type {
    match ty.kind(db) {
        TypeKind::Reference(_) => ty,
        TypeKind::Object(obj) => Type::new(
            db,
            TypeKind::Object(
                obj.iter()
                    .map(|(name, prop)| {
                        (
                            name.clone(),
                            Property {
                                ty: simplify_ty(db, prop.ty),
                                optional: prop.optional,
                            },
                        )
                    })
                    .collect(),
            ),
        ),
        TypeKind::Array(array_ty) => Type::new(db, TypeKind::Array(simplify_ty(db, array_ty))),
        TypeKind::Tuple(elements) => Type::new(
            db,
            TypeKind::Tuple(elements.iter().map(|ty| simplify_ty(db, *ty)).collect()),
        ),
        TypeKind::Or(options) => Type::new(
            db,
            TypeKind::Or(
                options
                    .iter()
                    .map(|opt| simplify_ty(db, *opt))
                    .sorted()
                    .dedup()
                    .collect(),
            ),
        ),
        TypeKind::And(options) => {
            let options = options
                .iter()
                .map(|opt| simplify_ty(db, *opt))
                .sorted()
                .dedup()
                .collect_vec();

            if options
                .iter()
                .all(|opt| matches!(opt.kind(db), TypeKind::Object(_)))
            {
                let mut fields = BTreeMap::new();

                for opt in options {
                    match opt.kind(db) {
                        TypeKind::Object(fs) => {
                            for (field, ty) in fs {
                                let Some(old) = fields.insert(field.clone(), ty) else {
                                    continue;
                                };
                                if let (TypeKind::Ident(_), TypeKind::String) =
                                    (old.ty.kind(db), ty.ty.kind(db))
                                {
                                    fields.insert(field, old);
                                }
                            }
                        }
                        _ => unreachable!(),
                    }
                }

                Type::new(db, TypeKind::Object(fields))
            } else {
                Type::new(db, TypeKind::And(options))
            }
        }
        TypeKind::Number
        | TypeKind::String
        | TypeKind::Boolean
        | TypeKind::Ident(_)
        | TypeKind::Null => ty,
    }
}
