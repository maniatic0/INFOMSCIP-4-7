
library(class);

# Utils
generate_points <- function(n, min_size, max_size) 
{
  X = runif(n, min=min_size, max=max_size);
  Y = runif(n, min=min_size, max=max_size);
  Color = replicate(length(X), 0);
  return (cbind(X, Y, Color))
};

norm_vector <- function(v)
{
  return (sqrt(sum(v^2)));
}

normalize_vector <- function(v) {
  len = norm_vector(v);
  if (len > 0)
  {
    return (v / len);
  }
  else
  {
    return (v);
  }
};

# In radians
angle_vectors <- function(v1, v2)
{
  n1 = normalize_vector(v1);
  n2 = normalize_vector(v2);
  dot = n1 %*% n2;
  return (acos(dot));
};

create_triangle <- function (p1, p2, p3)
{
  triangle = rbind(p1, p2, p3);
  colnames(triangle) = c("X", "Y");
  return (triangle);
};

get_triangle_angles <- function (triangle)
{
  p1 = triangle[1,1:2];
  p2 = triangle[2,1:2];
  p3 = triangle[3,1:2];
  
  alpha = angle_vectors(p2 - p1, p3 - p1);
  beta = angle_vectors(p1 - p2, p3 - p2);
  gamma = angle_vectors(p2 - p3, p1 - p3)
  
  return (c(alpha, beta, gamma));
};

get_triangle_base_height <- function (triangle)
{
  p1 = triangle[1,1:2];
  p2 = triangle[2,1:2];
  p3 = triangle[3,1:2];
  
  v1 = p2 - p1;
  v1_norm = norm_vector(v1);
  v2 = p3 - p2;
  v2_norm = norm_vector(v2);
  v3 = p1 - p3;
  v3_norm = norm_vector(v3);
  
  start = NULL;
  segment = NULL;
  other = NULL;
  
  if (v1_norm >= v2_norm)
  {
    if (v1_norm >= v3_norm)
    {
      start = p1;
      segment = v1;
      other = p3;
    }
    else
    {
      start = p3;
      segment = v3;
      other = p2;
    }
  }
  else
  {
    if (v2_norm >= v3_norm)
    {
      start = p2;
      segment = v2;
      other = p1;
    }
    else
    {
      start = p3;
      segment = v3;
      other = p2;
    }
  }
  
  res = {};
  res[["start"]] = start;
  res[["base"]] = segment;
  
  segment_norm = norm_vector(segment);
  segment_n = normalize_vector(segment);
  vother = other - start;
  
  dot_other = as.numeric(segment_n %*% vother);
  if (segment_norm > 0)
  {
    res[["height_base"]] = dot_other / segment_norm;
  }
  else
  {
    res[["height_base"]] = dot_other;
  }
   
  proy_other = segment_n * dot_other;
  res[["height"]] = vother - proy_other;
  
  res[["area"]] = norm_vector(res[["height"]]) * segment_norm / 2.0;
  
  return (res);
};

generate_triangle_base_height_decomp <- function (decomp, t)
{
  t_real = 0.5 + t * 0.5;
  
  p1 = decomp[["start"]];
  p2 = decomp[["start"]] + decomp[["base"]] * t_real + decomp[["height"]];
  p3 = decomp[["start"]] + decomp[["base"]];
  
  return (create_triangle(p1, p2, p3));
}

get_triangle_aabb <- function (triangle)
{
  min_aabb = c(min(triangle[,1]), min(triangle[,2]));
  max_aabb = c(max(triangle[,1]), max(triangle[,2]));
  
  aabb = rbind(min_aabb, max_aabb);
  colnames(aabb) = c("X", "Y");
  
  return (aabb);
};

triangle_is_inside <- function (triangle, min_size, max_size)
{
  aabb = get_triangle_aabb(triangle);
  
  min_ok = all(aabb >= min_size);
  max_ok = all(aabb <= max_size);
  
  return (min_ok & max_ok);
}

recenter_triangle <- function (triangle, min_size, max_size)
{
  center_size = c((min_size + max_size) / 2, (min_size + max_size) / 2);
  
  aabb = get_triangle_aabb(triangle);
  aabb_center = (aabb[1,] + aabb[2,]) / 2;
  
  dir = center_size - aabb_center;
  
  p1 = triangle[1,1:2];
  p2 = triangle[2,1:2];
  p3 = triangle[3,1:2];
  return (create_triangle(p1 + dir, p2 + dir, p3 + dir));
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
  p1 = triangle[1,1:2];
  p2 = triangle[2,1:2];
  p3 = triangle[3,1:2];
  
  d1 = triangle_half_plane_sign(point, p1, p2);
  d2 = triangle_half_plane_sign(point, p2, p3);
  d3 = triangle_half_plane_sign(point, p3, p1);
  
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

draw_points <- function(points, triangle, min_size, max_size, f, extra_str="")
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
  title(main=sprintf("Model%s n=%i f=%.5g", extra_str, p_len, f));
};

generate_model <- function(
  min_size, max_size, 
  triangle, 
  n, f, 
  plot_base=FALSE, extra_str="")
{
  points = generate_points(n, min_size, max_size);
  points = classify_triangle(points, triangle);
  points = generate_outliers(points, f);
  
  if (plot_base)
  {
    draw_points(points, triangle, min_size, max_size, f, extra_str);
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
# Utils

# Defaults
default_repeat = 20
default_min_size = 0
default_max_size = 10
default_triangle = create_triangle(c(3,3), c(7,3), c(7,7))
default_n = 500;
default_f = 0;
default_k = 5;
default_test_size = 10000;

default_triangle_decomp = get_triangle_base_height(default_triangle);
# Defaults

# Question Functions
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
    
    png(sprintf("first/n_%03i_boxplot.png", n));
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
  
  write.csv(results, file = "first/data.csv");
  
  png("first/error_percentage.png");
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
  
  png("first/correlation.png");
  cor1 = cor(results$n, results$`% Error Count Mean`);
  cor2 = cor(results$n, results$`% False Positives Count Mean`);
  cor3 = cor(results$n, results$`% False Negatives Count Mean`);
  corrs = c(cor1, cor2, cor3);
  barplot(
    corrs, 
    main="Correlation with n", 
    names.arg=c(
      sprintf("Errors (%%)\n%g", cor1), 
      sprintf("False Positives (%%)\n%g", cor2),  
      sprintf("False Negatives (%%)\n%g",cor3)
    ),
    ylab="Correlation",
    ylim = c(-1, 1)
  );
  dev.off();
  
  cor_res = data.frame(matrix(ncol = 3, nrow = length(1)));
  colnames(cor_res) = 
    c("Errors (%)", "False Positives (%)", "False Negatives (%)");
  rownames(cor_res) = c("Correlation with n");
  cor_res[1,] = corrs;
  write.csv(cor_res, file = "first/correlation.csv");
  
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
  
  write.csv(results, file = "second/data.csv");
  
  png("second/error_percentage.png");
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
  
  png("second/correlation.png");
  cor1 = cor(results$f, results$`% Error Count Mean`);
  cor2 = cor(results$f, results$`% False Positives Count Mean`);
  cor3 = cor(results$f, results$`% False Negatives Count Mean`);
  corrs = c(cor1, cor2, cor3);
  barplot(
    corrs, 
    main="Correlation with f", 
    names.arg=c(
      sprintf("Errors (%%)\n%g", cor1), 
      sprintf("False Positives (%%)\n%g", cor2),  
      sprintf("False Negatives (%%)\n%g",cor3)
      ),
    ylab="Correlation",
    ylim = c(-1, 1)
  );
  dev.off();
  
  
  cor_res = data.frame(matrix(ncol = 3, nrow = length(1)));
  colnames(cor_res) = 
    c("Errors (%)", "False Positives (%)", "False Negatives (%)");
  rownames(cor_res) = c("Correlation with f");
  cor_res[1,] = corrs;
  write.csv(cor_res, file = "second/correlation.csv");
  
  return(results);
};

perform_third_question <- function(t_values)
{
  triangles = list();
  for(i in 1:length(t_values))
  {
    t = t_values[i];
    triangle = generate_triangle_base_height_decomp(default_triangle_decomp, t);
    triangle = recenter_triangle(triangle, default_min_size, default_max_size);
    if (triangle_is_inside(triangle, default_min_size, default_max_size))
    {
      triangles[[i]] = triangle;
    }
    else
    {
      break;
    }
  }
  
  
  
  means_cols = c("Error Count Mean", "% Error Count Mean", "Correct Count Mean", 
                 "% Correct Mean",  "False Positives Count Mean", "% False Positives Count Mean", 
                 "False Negative Count Mean", "% False Negatives Count Mean");
  sd_cols = c("Error Count SD", "% Error Count SD", "Correct Count SD", 
              "% Correct SD",  "False Positives Count SD", "% False Positives SD", 
              "False Negative Count SD", "% False Negatives Count SD");
  cols = c(
    "t", means_cols, sd_cols);
  results = data.frame(matrix(ncol = length(cols), nrow = length(triangles)));
  colnames(results) = cols;
  
  for(i in 1:length(triangles))
  {
    triangle = triangles[[i]];
    t = t_values[i];
    result = perform_tests(
      default_min_size, default_max_size, triangle, 
      default_n, default_f, default_k, 
      default_test_size, default_repeat);
    results[i, "t"] = t;
    results[i, means_cols] = sapply(result, mean);
    results[i, sd_cols] = sapply(result, sd);
    
    png(sprintf("third/t_%03i_boxplot.png", i));
    boxplot(
      result["Error Count"], 
      ylab="Error Count", 
      main=sprintf(
        "t=%g mean=%g sd=%g", 
        t, 
        results[i, "Error Count Mean"],
        results[i, "Error Count SD"]
      )
    );
    dev.off();
    
    png(sprintf("third/t_%03i_model.png", i));
    generate_model(
      default_min_size, default_max_size, 
      triangle, 
      default_n, default_f, TRUE, sprintf(" t=%g", t)
    );
    
    angles = get_triangle_angles(triangle) * 180 / pi;
    alpha_str = sprintf("=%g°, ", angles[1]);
    beta_str = sprintf("=%g°, ", angles[2]);
    gamma_str = sprintf("=%g°", angles[3]);
    title(
      sub=bquote(
        alpha ~ .(alpha_str) ~
          beta ~ .(beta_str) ~ 
          gamma ~ .(gamma_str)
        )
    );
    
    dev.off();
  }
  
  write.csv(results, file = "third/data.csv");
  
  png("third/error_percentage.png");
  plot(
    results$t, 
    results$`% Error Count Mean`, 
    type='l', col="red", xlab = "t", ylab = "Errors (%)",
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
    results$t, 
    results$`% False Positives Count Mean`, 
    type='l', col="green");
  points(
    results$t, 
    results$`% False Negatives Count Mean`, 
    type='l', col="blue");
  legend("topleft", 
         legend=c(
           "Errors (%)", 
           "False Positives (%)",
           "False Negatives (%)"
         ), 
         col=c("red", "green", "blue"),
         lty=1
  );
  dev.off();
  
  png("third/correlation.png");
  cor1 = cor(results$t, results$`% Error Count Mean`);
  cor2 = cor(results$t, results$`% False Positives Count Mean`);
  cor3 = cor(results$t, results$`% False Negatives Count Mean`);
  corrs = c(cor1, cor2, cor3);
  barplot(
    corrs, 
    main="Correlation with t", 
    names.arg=c(
      sprintf("Errors (%%)\n%g", cor1), 
      sprintf("False Positives (%%)\n%g", cor2),  
      sprintf("False Negatives (%%)\n%g",cor3)
    ),
    ylab="Correlation",
    ylim = c(-1, 1)
  );
  dev.off();
  
  
  cor_res = data.frame(matrix(ncol = 3, nrow = length(1)));
  colnames(cor_res) = 
    c("Errors (%)", "False Positives (%)", "False Negatives (%)");
  rownames(cor_res) = c("Correlation with t");
  cor_res[1,] = corrs;
  write.csv(cor_res, file = "third/correlation.csv");
  
  return(results);
};

first_n_Values = c(100, 200, 300, 400, 500, 600, 700, 800);

second_f_values = c(0.0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3);

third_t_values = seq(0, 3, length.out=100); # 100 t values from 0 to 4

first_results = perform_first_question(first_n_Values);

second_results = perform_second_question(second_f_values);

third_results = perform_third_question(third_t_values);


