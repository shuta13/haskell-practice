# practice
## stack使い方メモ
インストール : `brew install stack`
### ghci起動
`stack exec ghci`
### プロジェクト作成
`stack new app-hoge`
### コンパイル&実行
`stack runghc -- -isrc app/Main.hs`
(`-- isrc`で`Lib`の重複を無視、本来は`stack runghc app/Main.hs`)
### コンパイルだけ
`stack ghc app/Main.hs`
(Libの重複無視するなら`-- -isrc`必要)