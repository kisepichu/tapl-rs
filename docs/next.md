# 次の機能実装の概要

web playground のパーツとしてこのプロジェクトを使えるようにします。

## web playground の機能概要

- web playground は React などで SPA として作り、左にコード編集部分があり右に typst 導出図があるようにします。
- コード編集部分では言語や評価戦略を変えるプルダウンがあり、言語は今のところ simplebase と simplelambdamu を予定しています。
- 言語変更をすると、それに対応した wasm が遅延ロードされます。
- コード編集部分を変更すると、導出図が更新されます。
  - 導出図の描画は、 typst-ts という typescript 側のライブラリでやるためこのプロジェクトには関係ない。
- 評価ボタンを押すと、左のコード編集部分の下部に評価結果が表示され、右の下にも簡約後の導出図が表示されます。

## それに必要なこのプロジェクトの機能

- 各言語ごとに同じインターフェイスで WASM で ts から呼び出せる API を提供する。
  - コードが変更された時の、パースして型付け結果と導出図の typst コードを返す API.
  - 評価ボタンを押された時の、パースして評価結果と評価後の導出図の typst コードを返す API.
    - 引数はコードと評価戦略文字列。

## ディレクトリ構成(暫定)

tapl-rs
├─ languages/
│   ├─ simplebase/
│   ├─ fullsimple/
│   ├─ simplelambdamu/
│   ├─ ... 今あるすべての言語
─ wasm/                 
    ├─ simplebase-wasm/
    │   ├─ src/lib.rs 
    │   ├─ Cargo.toml
    ├─ simplelambdamu-wasm/

(参考 web playground 側別リポジトリの構造(暫定))

tapl-playground/           # Web UI
├─ package.json
├─ vite.config.ts
├─ src/
│   ├─ features/
│   │   ├─ editor/
│   │   ├─ language-switcher/
│   │   ├─ preview-pane/
│   │   └─ ...
│   ├─ libs/tapl-rs? # ここに git submodule で入れる
│   ├─ pages/
│   ├─ app.tsx
│   └─ main.tsx
└─ public/

## todo

- next.md の内容の実現可能性
- 特に遅延ロードらへん、 Rust で trait とか使って各言語の wasm の API を統一してとして、 ts 側からも言語を切り返したときに各 API が型安全に呼べるのかのような部分は考慮したい。
- まずは今ある言語たちを language 内に入れてもらって cargo.toml を更新して動くか確認してもらう?
