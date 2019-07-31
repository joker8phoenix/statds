# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'
#
#' @param [param] [description]:引数の説明
#' @importFrom [package] [func]:外部パッケージを読み込む。usethis::use_package("name")を実行することでDESCRIPTIONに依存パッケージが記述される。
#' @examples:関数の使用例
#' @export:ユーザーが使う関数。パッケージ内部の処理で使用する関数なら必要なし。
hello <- function() {
  print("Hello, world!")
}
