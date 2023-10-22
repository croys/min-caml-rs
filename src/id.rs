// 変数の名前 - variable name
#[derive(Clone, PartialEq, Debug)]
pub struct T(String);

// トップレベル関数やグローバル配列のラベル
//   - labels for top level function and global arrays
#[derive(Clone, PartialEq, Debug)]
pub struct L(String);
