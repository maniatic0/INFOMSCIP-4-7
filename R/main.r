generate_points <- function(n, min_size, max_size) 
{
  X = runif(n, min=min_size, max=max_size);
  Y = runif(n, min=min_size, max=max_size);
  Color = replicate(length(X), 0);
  return (cbind(X, Y, Color))
}

create_triangle <- function (p1, p2, p3)
{
  return (rbind(p1, p2, p3));
}

triangle_half_plane_sign <- function(p1, p2, p3)
{
  # From https://stackoverflow.com/a/2049593
  # Basically a 2D outer product
  return ((p1[1] - p3[1]) * (p2[2] - p3[2]) - (p2[1] - p3[1]) * (p1[2] - p3[2]));
}

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
}

classify_triangle <- function(points, triangle)
{
  p_len = dim(points)[1];
  points[1:p_len, 3] = apply(points, c(1), is_inside_triangle, triangle);
  return (points);
}

draw_points <- function(points, triangle, min_size, max_size)
{
  p_len = dim(points)[1];
  plot(
    points[1:p_len, 1:2], 
    col=ifelse(points[1:p_len, 3],"blue","red"), 
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
  points(v1[1], v1[2], col="green", lwd=2, pch=16);
  points(v2[1], v2[2], col="green", lwd=2, pch=16);
  points(v3[1], v3[2], col="green", lwd=2, pch=16);
  legend("bottomright", 
         legend=c(
           "Base In", 
           "Base Out", 
           sprintf("(%.2g, %.2g)", v1[1], v1[2]), 
           sprintf("(%.2g, %.2g)", v2[1], v2[2]), 
           sprintf("(%.2g, %.2g)", v3[1], v3[2])
          ), 
         col=c("blue", "red", "green", "green", "green"), 
         pch=16
  );
}


default_min_size = 0
default_max_size = 10

default_triangle = create_triangle(c(3,3), c(7,3), c(7,7))

points = generate_points(1000, default_min_size, default_max_size)

points = classify_triangle(points, default_triangle)
draw_points(points, default_triangle, default_min_size, default_max_size)


