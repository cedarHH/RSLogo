use crate::utils::ast::ast_build;
use crate::utils::ast_definition::Ast;
use crate::utils::interpreter::Interpreter;
use crate::utils::lexer::lexical_analyzer;
use crate::utils::turtle::Turtle;
use unsvg::Image;

mod utils;

/// # RSLogo
/// Parsing RSLogo Scripts and Drawing in Images
/// ### Arguments
/// - `file_content` - A string containing the RSLogo script to be executed.
/// - `image` - A mutable reference to an `usvg::Image` object where the drawing will be rendered.
/// - `height` - The height of the output image in pixels.
/// - `width` - The width of the output image in pixels.
/// ### Returns
/// - returns `Ok(())`
///     - If the RSLogo scripts executes successfully, this function will drawing the specified graphics on the image.
/// - returns `Err(String)`
///     - If there is an error during parsing, analyzing, or executing the script, this function will return `Err(String)` with the `String` providing details about the nature of the error
/// ### Examples
///
/// ``` rust
/// use unsvg::Image;
/// use rslogo::run;
/// let file_content = String::from("PENUP\nFORWARD \"50\nPENDOWN\nFORWARD \"50"); // Example script
/// let mut image = Image::new(200, 200); // Placeholder for the actual image object
/// let result = run(file_content, &mut image, 200, 200);
/// assert!(result.is_ok());
///
/// ```
pub fn run(file_content: String, image: &mut Image, width: u32, height: u32) -> Result<(), String> {
    let turtle = Turtle::new(width as f32 / 2.0, height as f32 / 2.0, image);
    let pairs = lexical_analyzer(&file_content)?;
    let mut ast = Ast::new(&file_content);
    ast_build(&mut ast, pairs);
    let mut interpreter = Interpreter::new(turtle);
    interpreter.execute(&ast)?;
    Ok(())
}
