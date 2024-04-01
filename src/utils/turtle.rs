use unsvg::{get_end_coordinates, Image, COLORS};

// Store the state of the turtle and Image
pub struct Turtle<'a> {
    pen: bool,
    pub xcor: f32,
    pub ycor: f32,
    pub heading: i32,
    pub color: usize,
    image: &'a mut Image,
}

impl<'a> Turtle<'a> {
    pub fn new(xcor: f32, ycor: f32, image: &'a mut Image) -> Self {
        Turtle {
            pen: false,
            heading: 0,
            color: 7,
            xcor,
            ycor,
            image,
        }
    }

    // Set the state of the turtle.
    // PENUP PENDOWN SETPENCOLOR SETHEADING SETX SETY TURN
    pub fn set_values(
        &mut self,
        pen: Option<bool>,
        xcor: Option<f32>,
        ycor: Option<f32>,
        heading: Option<i32>,
        color: Option<usize>,
    ) {
        if let Some(xcor) = xcor {
            self.xcor = xcor;
        }
        if let Some(ycor) = ycor {
            self.ycor = ycor;
        }
        if let Some(heading) = heading {
            self.heading = heading;
        }
        if let Some(pen) = pen {
            self.pen = pen;
        }
        if let Some(color) = color {
            self.color = color;
        }
    }

    // Control the turtle to move.
    // FORWARD BACK LEFT RIGHT
    pub fn action(&mut self, distance: f32, degree: i32) {
        if self.pen {
            if let Ok(res) = self.image.draw_simple_line(
                self.xcor,
                self.ycor,
                self.heading + degree,
                distance,
                COLORS[self.color],
            ) {
                (self.xcor, self.ycor) = res;
            };
        } else {
            (self.xcor, self.ycor) =
                get_end_coordinates(self.xcor, self.ycor, self.heading + degree, distance);
        }
    }
}
