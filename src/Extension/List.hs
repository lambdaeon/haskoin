module Extension.List where


safeLast :: [a] -> Maybe a
safeLast xs =
  -- {{{
  case reverse xs of
    x : _ ->
      Just x
    _ ->
      Nothing
  -- }}}
