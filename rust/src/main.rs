use image::GenericImageView;
use std::path::PathBuf;
use structopt::StructOpt;

mod beliefpropagation;
mod common;
mod data;
mod smoothness;

#[derive(Debug)]
enum Algorithms {
    SAD,
    SSD,
    DynamicProgramming,
    BeliefPropagation,
}

impl std::str::FromStr for Algorithms {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "sad" => Ok(Algorithms::SAD),
            "ssd" => Ok(Algorithms::SSD),
            "dynamicprogramming" | "dynamic_programming" | "dynamic-programming" | "dp" => {
                Ok(Algorithms::DynamicProgramming)
            }
            "beliefpropagation" | "belief_propagation" | "belief-propagation" | "bp" => {
                Ok(Algorithms::BeliefPropagation)
            }
            _ => Err(
                "Parsing of the algorithm failed.  Please specify one of the possible options."
                    .to_string(),
            ),
        }
    }
}

impl std::fmt::Display for Algorithms {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let algo_string = match self {
            Algorithms::SAD => "sad",
            Algorithms::SSD => "ssd",
            Algorithms::DynamicProgramming => "dynamic-programming",
            Algorithms::BeliefPropagation => "belief-propagation",
        };
        write!(f, "{}", algo_string)
    }
}

#[derive(Debug, StructOpt)]
#[structopt(
    name = "Practice stereo matcher",
    about = "A command-line program to execute various stereo matching algorithms on stereo pairs of images."
)]
struct CLIParameters {
    #[structopt(
        short = "l",
        parse(from_os_str),
        help = "Path to the left image to be matched on."
    )]
    left_image_path: PathBuf,
    #[structopt(
        short = "r",
        parse(from_os_str),
        help = "Path to the right image to be matched on."
    )]
    right_image_path: PathBuf,
    #[structopt(
        short = "o",
        parse(from_os_str),
        help = "Path to the output directory for the disparity map to be output to.\nNote that you DO NOT provide a filename, just the directory.  The filename is generated by the program itself."
    )]
    output_directory: PathBuf,
    #[structopt(
        short = "w",
        help = "The size of a side of the square window you would like to use for the matching.  Currently only relevant to SAD and SSD."
    )]
    window_size: Option<usize>,
    #[structopt(
        short = "d",
        help = "The maximum disparity size that the program will search out to (defaults to 32)."
    )]
    maximum_disparity: Option<usize>,
    #[structopt(
        short = "a",
        help = "The algorithm you would like to use for the stereo matching."
    )]
    algorithm: Algorithms,
    #[structopt(
        short = "n",
        help = "The number of iterations of the algorithm to carry out.  Currently only relevant to belief propagation."
    )]
    number_of_iterations: Option<usize>,
    #[structopt(
        short = "z",
        help = "Set this flag if you would like the program to use the zero-mean version of the unary cost function.  (Currently does nothing)"
    )]
    use_zero_mean: bool,
    #[structopt(
        short = "png",
        help = "Set this flag if you would like the program to save the output image as a png.  Otherwise, it defaults to using the same file extension as on the left input image.  (Currently does nothing)"
    )]
    save_as_png: bool,
}

fn determine_output_file_path(params: &CLIParameters) -> PathBuf {
    let algorithm_string = format!("{}", params.algorithm);
    let left_image_name_without_extension = params
        .left_image_path
        .file_stem()
        .expect("Left image name apparently has no file name (???)")
        .to_str()
        .unwrap();

    let left_image_extension = {
        if params.save_as_png {
            ".png"
        } else {
            params
                .left_image_path
                .extension()
                .expect("Left image name apparently has no extension (???)")
                .to_str()
                .unwrap()
        }
    };

    let window_size = if params.window_size.is_some() {
        format!("{}", params.window_size.unwrap())
    } else {
        String::new()
    };

    let number_of_iterations = if params.number_of_iterations.is_some() {
        format!("{}", params.number_of_iterations.unwrap())
    } else {
        String::new()
    };

    let mut filename = params.output_directory.to_owned();

    filename.push(
        &[
            "rust",
            left_image_name_without_extension,
            &algorithm_string,
            &window_size,
            &number_of_iterations,
        ]
        .join("_"),
    );

    filename.set_extension(left_image_extension);
    filename
}

fn main() {
    let cli_parameters = CLIParameters::from_args();
    assert!(cli_parameters.output_directory.is_dir());
    let output_filename = determine_output_file_path(&cli_parameters);

    let start_time = std::time::Instant::now();

    let left_image = image::open(cli_parameters.left_image_path).expect("Couldn't open left image");

    let (image_width, image_height) = left_image.dimensions();

    let left_image_buffer = left_image.grayscale().raw_pixels();
    let right_image_buffer = image::open(cli_parameters.right_image_path)
        .expect("Couldn't open right image")
        .grayscale()
        .raw_pixels();
    let parameters = common::Parameters {
        left_image: &left_image_buffer,
        right_image: &right_image_buffer,
        width: image_width as usize,
        height: image_height as usize,
        total_pixels: (image_width as usize) * (image_height as usize),
        window_edge_size: cli_parameters.window_size.unwrap_or(3),
        maximum_disparity: cli_parameters.maximum_disparity.unwrap_or(32),
        use_zero_mean: cli_parameters.use_zero_mean,
    };

    let algo_start_time = std::time::Instant::now();

    let output_vec = {
        match cli_parameters.algorithm {
            Algorithms::SAD => unimplemented!(),
            Algorithms::SSD => unimplemented!(),
            Algorithms::DynamicProgramming => unimplemented!(),
            Algorithms::BeliefPropagation => {
                let bpparameters = beliefpropagation::BPParameters {
                    number_of_iterations: cli_parameters.number_of_iterations.unwrap_or(10),
                    data_cost_function: |a, b| {
                        data::truncated_linear_f32_fh(common::LAMBDA_FH, common::TAU_FH, a, b)
                    },
                    smoothness_cost_function: |a, b| {
                        smoothness::truncated_linear_f32_fh(common::D_FH, a, b)
                    },
                };
                beliefpropagation::belief_propagation(&parameters, &bpparameters)
            }
        }
    };

    let algo_end_time = std::time::Instant::now();

    let mut output_image = image::ImageBuffer::<image::Luma<u8>, Vec<u8>>::from_raw(
        image_width,
        image_height,
        output_vec,
    )
    .expect("Error creating the output image!");
    imageproc::contrast::equalize_histogram_mut(&mut output_image);

    assert!(output_image.save(output_filename).is_ok());

    let end_time = std::time::Instant::now();

    println!(
        "Algorithm running time was {:?}.",
        algo_end_time.duration_since(algo_start_time)
    );
    println!(
        "Full running time was {:?}.",
        end_time.duration_since(start_time)
    );
}
