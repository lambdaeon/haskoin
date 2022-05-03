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


duplicateLast :: [a] -> [a]
duplicateLast xs =
  -- {{{
  case safeLast xs of
    Nothing ->
      xs
    Just lastX ->
      xs ++ [lastX]
  -- }}}
