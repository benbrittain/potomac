use std::sync::Arc;

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

#[derive(Debug, Diagnostic, Clone, Eq, PartialEq, Error)]
#[error("Toy failure.")]
pub struct ToyFailure {
    /// Original input that this failure came from.
    #[source_code]
    pub input: Arc<String>,

    /// Sub-diagnostics for this failure.
    #[related]
    pub diagnostics: Vec<ToyDiagnostic>,
}

/// An individual diagnostic message for a Toy parsing issue.
#[derive(Debug, Diagnostic, Clone, Eq, PartialEq, Error)]
#[error("{kind}")]
pub struct ToyDiagnostic {
    /// Shared source for the diagnostic.
    #[source_code]
    pub input: Arc<String>,

    /// Offset in chars of the error.
    #[label("{}", label.unwrap_or("here"))]
    pub span: SourceSpan,

    /// Label text for this span. Defaults to `"here"`.
    pub label: Option<&'static str>,

    /// Suggestion for fixing the parser error.
    #[help]
    pub help: Option<String>,

    /// Specific error kind for this parser error.
    pub kind: ToyErrorKind,
}

/// A type reprenting additional information specific to the type of error being returned.
#[derive(Debug, Diagnostic, Clone, Eq, PartialEq, Error)]
pub enum ToyErrorKind {
    #[error("Failed to parse source code (lexer).")]
    #[diagnostic(code(toy::lex))]
    Lex,
    #[error("Failed to parse source code (parser).")]
    #[diagnostic(code(toy::parse))]
    Parse,
}
