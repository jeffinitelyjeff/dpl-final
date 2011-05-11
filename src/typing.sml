structure Typing = 
struct
  exception infer_error
  type var = string

  datatype t = V of var | Int | Bool | Arrow of t*t | List of t

  fun infer arrrrg = raise infer_error

  fun toString arrrrg = raise infer_error
end
