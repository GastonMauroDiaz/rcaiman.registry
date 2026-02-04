.check_vector <- function(x,
                          type = c("numeric", "character", "logical",
                                   "integerish", "even_integerish",
                                   "date"),
                          length = NULL,
                          allow_null = FALSE,
                          sign = c("any", "positive", "nonnegative",
                                   "negative", "nonpositive"),
                          name = deparse(substitute(x))
) {
  .is_integerish <- function(v) {
    if (is.list(v)) return(FALSE)
    if (!is.vector(v) || !is.numeric(v)) return(FALSE)
    if (any(is.infinite(v))) return(FALSE)
    all(round(v) == v)
  }
  .is_even <- function(v) {
    if (!is.vector(v) || !is.numeric(v)) return(FALSE)
    all(round(v/2) == v/2)
  }

  if (is.null(x)) {
    if (allow_null) return(invisible(TRUE))
    stop(sprintf("`%s` cannot be `NULL`.", name), call. = FALSE)
  }
  if (is.list(x)) {
    stop(sprintf("`%s` must be an atomic vector, not a list.", name),
         call. = FALSE)
  }

  type <- match.arg(type)
  sign <- match.arg(sign)

  ok_type <- switch(
    type,
    numeric         = is.vector(x) && is.numeric(x),
    character       = is.vector(x) && is.character(x),
    logical         = is.vector(x) && is.logical(x),
    integerish      = .is_integerish(x),
    even_integerish = .is_integerish(x) && .is_even(x),
    date            = inherits(x, "Date")
  )

  # Mensaje base por tipo
  type_msg <- switch(
    type,
    numeric         = "a numeric vector",
    character       = "a character vector",
    logical         = "a logical vector",
    integerish      = "an integer-like numeric vector",
    even_integerish = "an even integer-like numeric vector",
    date            = "a Date object (use as.Date())"
  )

  # Frase de longitud
  len_msg <- if (!is.null(length)) sprintf(" of length %d", length) else ""

  # Frase de signo
  sign_msg <- switch(
    sign,
    any          = "",
    positive     = " with positive values only",
    nonnegative  = " with non-negative values only",
    negative     = " with negative values only",
    nonpositive  = " with non-positive values only"
  )

  # Error de tipo
  if (!ok_type) {
    stop(sprintf("`%s` must be %s%s%s.", name, type_msg, len_msg, sign_msg),
         call. = FALSE)
  }

  # Error de longitud
  if (!is.null(length) && length(x) != length) {
    stop(sprintf("`%s` must be %s%s%s.", name, type_msg, len_msg, sign_msg),
         call. = FALSE)
  }

  # Chequeo de signo (solo tipos numéricos o integerish)
  if (sign != "any" && type %in% c("numeric","integerish","even_integerish")) {
    vals <- if (is.numeric(x)) x else as.numeric(x)
    ok_sign <- switch(
      sign,
      positive     = all(vals > 0),
      nonnegative  = all(vals >= 0),
      negative     = all(vals < 0),
      nonpositive  = all(vals <= 0)
    )
    if (!isTRUE(ok_sign)) {
      stop(sprintf("`%s` must be %s%s%s.", name, type_msg, len_msg, sign_msg),
           call. = FALSE)
    }
  }

  invisible(TRUE)
}

.assert_choice <- function(x, allowed,
                           name = deparse(substitute(x)),
                           allow_null = FALSE,
                           multiple = FALSE,
                           case_sensitive = TRUE) {
  # sanity on allowed
  if (is.null(allowed) || !is.character(allowed) || length(allowed) < 1 || anyNA(allowed))
    stop("`allowed` must be a non-empty character vector without `NA`.", call. = FALSE)

  q <- function(v) sprintf('"%s"', v)
  collapse_or <- function(v) {
    vq <- q(v)
    n <- length(vq)
    if (n == 1) vq
    else if (n == 2) paste(vq, collapse = " or ")
    else paste0(paste(vq[-n], collapse = ", "), ", or ", vq[n])
  }

  if (is.null(x)) {
    if (allow_null) return(invisible(TRUE))
    stop(sprintf("`%s` cannot be `NULL`.", name), call. = FALSE)
  }

  if (!is.character(x))
    stop(sprintf("`%s` must be a character vector.", name), call. = FALSE)
  if (anyNA(x))
    stop(sprintf("`%s` cannot contain `NA`.", name), call. = FALSE)

  if (!multiple && length(x) != 1)
    stop(sprintf("`%s` must be a character vector of length one.", name), call. = FALSE)

  # case handling
  if (!case_sensitive) {
    x <- tolower(x)
    allowed <- tolower(allowed)
  }

  ok <- if (multiple) all(x %in% allowed) else x %in% allowed

  if (!ok) {
    if (multiple) {
      stop(sprintf("`%s` must contain only allowed values: %s.",
                   name, paste(q(allowed), collapse = ", ")), call. = FALSE)
    } else {
      msg <- if (length(allowed) == 2)
        sprintf("either %s", collapse_or(allowed))
      else
        sprintf("one of: %s", paste(q(allowed), collapse = ", "))
      stop(sprintf("`%s` must be %s.", name, msg), call. = FALSE)
    }
  }

  invisible(TRUE)
}

.assert_id <- function(id, name = deparse(substitute(id))) {
  if (!grepl("^[a-z0-9_]+$", id)) {
    stop(
      sprintf("`%s`  must use snake_case and contain only lowercase letters, numbers, and underscores.", name)
    )
  }
}

.is_geometry_spec <- function(x) {
  is.list(x) &&
    !is.null(x$radiometry) &&
    is.list(x$radiometry)
}

.is_embedded_metadata_identity <- function(x) {
  is.list(x) &&
    !is.null(x$namespace) &&
    !is.null(x$rules) &&
    is.list(x$rules)
}

.is_file_identity <- function(x) {
  is.list(x) && !is.null(x$extension)
}

.split_registry_components <- function(registry) {
  stopifnot(inherits(registry, "hemispherical_camera_registry"))

  ## Instrument identity is ontologically unique and excluded
  components <- registry[names(registry) != "instrument_identity"]

  is_geometry  <- vapply(components, .is_geometry_spec, logical(1))
  is_embedded  <- vapply(components, .is_embedded_metadata_identity, logical(1))
  is_file      <- vapply(components, .is_file_identity, logical(1))

  list(
    components            = components,
    # is_geometry            = is_geometry,
    # is_embedded_metadata   = is_embedded,
    # is_file_identity       = is_file,
    geometry_ids           = names(components)[is_geometry],
    embedded_metadata_ids  = names(components)[is_embedded],
    file_identity_ids      = names(components)[is_file]
  )
}

