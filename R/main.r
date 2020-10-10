
library(class);

generate_points <- function(n, min_size, max_size) 
{
  X = runif(n, min=min_size, max=max_size);
  Y = runif(n, min=min_size, max=max_size);
  Color = replicate(length(X), 0);
  return (cbind(X, Y, Color))
};

create_triangle <- function (p1, p2, p3)
{
  return (rbind(p1, p2, p3));
};

triangle_half_plane_sign <- function(p1, p2, p3)
{
  # From https://stackoverflow.com/a/2049593
  # Basically a 2D outer product
  return ((p1[1] - p3[1]) * (p2[2] - p3[2]) - (p2[1] - p3[1]) * (p1[2] - p3[2]));
};

is_inside_triangle <- function(point, triangle)
{
  # From https://stackoverflow.com/a/2049593
  # Half plane trick
  v1 = triangle[1,1:2];
  v2 = triangle[2,1:2];
  v3 = triangle[3,1:2];
  
  d1 = triangle_half_plane_sign(point, v1, v2);
  d2 = triangle_half_plane_sign(point, v2, v3);
  d3 = triangle_half_plane_sign(point, v3, v1);
  
  has_neg = (d1 < 0) || (d2 < 0) || (d3 < 0);
  has_pos = (d1 > 0) || (d2 > 0) || (d3 > 0);
  
  return (!(has_neg && has_pos));
};

classify_triangle <- function(points, triangle)
{
  points[, 3] = apply(points, c(1), is_inside_triangle, triangle);
  return (points);
};

generate_outliers <- function(points, f)
{
  probable_flip <- function(point, f)
  {
    to_flip = runif(1) < f;
    if(to_flip)
    {
      return(!point[3]);
    }
    else
    {
      return(point[3]); 
    }
  };
  points[, 3] = apply(points, c(1), probable_flip, f);
  return (points);
}

draw_points <- function(points, triangle, min_size, max_size, f)
{
  p_len = dim(points)[1];
  plot(
    points[, 1:2], 
    col=ifelse(points[, 3],"blue","red"), 
    xlim=c(min_size, max_size), 
    ylim=c(min_size,max_size),
    pch=16
  );
  
  v1 = triangle[1,1:2];
  v2 = triangle[2,1:2];
  v3 = triangle[3,1:2];
  segments(v1[1], v1[2], v2[1], v2[2], col="black", lwd=2);
  segments(v2[1], v2[2], v3[1], v3[2], col="black", lwd=2);
  segments(v3[1], v3[2], v1[1], v1[2], col="black", lwd=2);
  points(v1[1], v1[2], col="yellow", lwd=2, pch=16);
  points(v2[1], v2[2], col="green", lwd=2, pch=16);
  points(v3[1], v3[2], col="cyan", lwd=2, pch=16);
  legend("bottomright", 
         legend=c(
           "Base In", 
           "Base Out", 
           sprintf("(%.2g, %.2g)", v1[1], v1[2]), 
           sprintf("(%.2g, %.2g)", v2[1], v2[2]), 
           sprintf("(%.2g, %.2g)", v3[1], v3[2])
          ), 
         col=c("blue", "red", "yellow", "green", "cyan"), 
         pch=16
  );
  title(main=sprintf("Model n=%i f=%.5g", p_len, f));
};

generate_model <- function(min_size, max_size, triangle, n, f, plot_base=FALSE)
{
  points = generate_points(n, min_size, max_size);
  points = classify_triangle(points, triangle);
  points = generate_outliers(points, f);
  
  if (plot_base)
  {
    draw_points(points, triangle, min_size, max_size, f);
  }
  
  return (points);
};

test_model <- function(min_size, max_size, triangle, n, f, k, test_size)
{
  points = generate_model(min_size, max_size, triangle, n, f);
  
  tests = generate_points(test_size, min_size, max_size);
  tests = classify_triangle(tests, triangle);
  
  prediction = knn(points[,1:2], tests[,1:2], cl=factor(points[,3]), k=k);
  prediction = prediction == 1;
  
  results = {};
  
  results["Error Count"] = sum(prediction != tests[,3]);
  results["% Error Count"] = (results["Error Count"] / test_size) * 100.0;
  
  results["Correct Count"] = test_size - results["Error Count"];
  results["% Correct"] = (results["Correct Count"] / test_size) * 100.0;
  
  results["False Positives Count"] = sum((!prediction) & tests[,3]);
  results["% False Positives"] =
    (results["False Positives Count"] / test_size) * 100.0;
  
  results["False Negative Count"] = sum(prediction & (!tests[,3]));
  results["% False Negatives"] = 
    (results["False Negative Count"] / test_size) * 100.0;
  
  return(results);
};

perform_tests <- function(min_size, max_size, triangle, n, f, k, test_size, repeats)
{
  cols = c(
    "Error Count", "% Error Count", "Correct Count", 
    "% Correct",  "False Positives Count", "% False Positives", 
    "False Negatives Count", "% False Negatives Count");
  results = data.frame(matrix(ncol = length(cols), nrow = repeats));
  colnames(results) = cols;
  
  for(i in 1:repeats)
  {
    results[i,] =
      test_model(min_size, max_size, triangle, n, f, k, test_size);
  }
  return(results);
};

default_repeat = 20
default_min_size = 0
default_max_size = 10
default_triangle = create_triangle(c(3,3), c(7,3), c(7,7))
default_n = 500;
default_f = 0;
default_k = 5;
default_test_size = 10000;

perform_first_question <- function(n_values)
{
  means_cols = c("Error Count Mean", "% Error Count Mean", "Correct Count Mean", 
  "% Correct Mean",  "False Positives Count Mean", "% False Positives Count Mean", 
  "False Negative Count Mean", "% False Negatives Count Mean");
  sd_cols = c("Error Count SD", "% Error Count SD", "Correct Count SD", 
    "% Correct SD",  "False Positives Count SD", "% False Positives SD", 
    "False Negative Count SD", "% False Negatives Count SD");
  cols = c(
    "n", means_cols, sd_cols);
  results = data.frame(matrix(ncol = length(cols), nrow = length(n_values)));
  colnames(results) = cols;
  
  for(i in 1:length(n_values))
  {
    n = n_values[i];
    result = perform_tests(
      default_min_size, default_max_size, default_triangle, 
      n, default_f, default_k, 
      default_test_size, default_repeat);
    results[i, "n"] = n;
    results[i, means_cols] = sapply(result, mean);
    results[i, sd_cols] = sapply(result, sd);
    
    png(sprintf("first/n_%i_boxplot.png", n));
    boxplot(
      result["Error Count"], 
      ylab="Error Count", 
      main=sprintf(
        "n=%i mean=%g sd=%g", 
        n, 
        results[i, "Error Count Mean"],
        results[i, "Error Count SD"]
        )
      );
    dev.off();
  }
  write.csv(results, file = "first/data.csv")
  png("first/error_percentage.png")
  plot(
    results$n, 
    results$`% Error Count Mean`, 
    type='l', col="red", xlab = "n", ylab = "Errors (%)",
    ylim = c(
      min(
        results$`% Error Count Mean`, 
        results$`% False Positives Count Mean`, 
        results$`% False Negatives Count Mean`
        ), 
      max(
        results$`% Error Count Mean`, 
        results$`% False Positives Count Mean`, 
        results$`% False Negatives Count Mean`
        )
      )
    );
  points(
    results$n, 
    results$`% False Positives Count Mean`, 
    type='l', col="green");
  points(
    results$n, 
    results$`% False Negatives Count Mean`, 
    type='l', col="blue");
  legend("topright", 
         legend=c(
           "Errors (%)", 
           "False Positives (%)",
           "False Negatives (%)"
         ), 
         col=c("red", "green", "blue"),
         lty=1
  );
  dev.off();
  return(results);
};

perform_second_question <- function(f_values)
{
  means_cols = c("Error Count Mean", "% Error Count Mean", "Correct Count Mean", 
                 "% Correct Mean",  "False Positives Count Mean", "% False Positives Count Mean", 
                 "False Negative Count Mean", "% False Negatives Count Mean");
  sd_cols = c("Error Count SD", "% Error Count SD", "Correct Count SD", 
              "% Correct SD",  "False Positives Count SD", "% False Positives SD", 
              "False Negative Count SD", "% False Negatives Count SD");
  cols = c(
    "f", means_cols, sd_cols);
  results = data.frame(matrix(ncol = length(cols), nrow = length(f_values)));
  colnames(results) = cols;
  
  for(i in 1:length(f_values))
  {
    f = f_values[i];
    result = perform_tests(
      default_min_size, default_max_size, default_triangle, 
      default_n, f, default_k, 
      default_test_size, default_repeat);
    results[i, "f"] = f;
    results[i, means_cols] = sapply(result, mean);
    results[i, sd_cols] = sapply(result, sd);
    
    png(sprintf("second/f_%i_boxplot.png", i));
    boxplot(
      result["Error Count"], 
      ylab="Error Count", 
      main=sprintf(
        "f=%g mean=%g sd=%g", 
        f, 
        results[i, "Error Count Mean"],
        results[i, "Error Count SD"]
      )
    );
    dev.off();
  }
  write.csv(results, file = "second/data.csv")
  png("second/error_percentage.png")
  plot(
    results$f, 
    results$`% Error Count Mean`, 
    type='l', col="red", xlab = "f", ylab = "Errors (%)",
    ylim = c(
      min(
        results$`% Error Count Mean`, 
        results$`% False Positives Count Mean`, 
        results$`% False Negatives Count Mean`
      ), 
      max(
        results$`% Error Count Mean`, 
        results$`% False Positives Count Mean`, 
        results$`% False Negatives Count Mean`
      )
    )
  );
  points(
    results$f, 
    results$`% False Positives Count Mean`, 
    type='l', col="green");
  points(
    results$f, 
    results$`% False Negatives Count Mean`, 
    type='l', col="blue");
  legend("topright", 
         legend=c(
           "Errors (%)", 
           "False Positives (%)",
           "False Negatives (%)"
         ), 
         col=c("red", "green", "blue"),
         lty=1
  );
  dev.off();
  return(results);
};

#first_n_Values = c(100, 200, 300, 400, 500, 600, 700, 800);

second_f_values = c(0.0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3);

#first_results = perform_first_question(first_n_Values);

second_results = perform_second_question(second_f_values);



