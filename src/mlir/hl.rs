#![allow(unused_variables)]
use sys::{
    MlirDiagnosticHandler as DiagnosticHandler, MlirDiagnosticHandlerID as DiagnosticHandlerID,
    MlirOperationWalkCallback as OperationWalkCallback, MlirStringCallback as StringCallback,
    MlirWalkOrder as WalkOrder,
};

use crate::mlir::sys;
include!(concat!(env!("OUT_DIR"), "/high-level.rs"));
