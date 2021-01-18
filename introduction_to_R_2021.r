####
# Rであそぼ
# 2021年1月14日　
# 宇都宮　譲（長崎大学経済学部）
#### 

####
# 基本的な使い方
# 1.　コードを書く
# 2.　実行したい部分を、矢印＋Shiftで選択する
# 3.　Ctrl + Enterで実行
#
# 困ったときは
# 1. 関数名に?をつけて実行　例）?ggplot2::ggplot
# 2. Googleなどで調べる
#
# コツ
# 1. コメントを書き込む：メモを書いて、メモ部分を選択してCtrl + shift + c
# 2. なにか書いたらすぐにCtrl+sで上書き保存
####

#### Rコード
# 1. ライブラリ（パッケージ）を読み込む
library(tidyverse) # tidyなデータ処理や描画に関するライブラリ詰め合わせ
library(lubridate) # 日付データを手頃にまともに扱えるライブラリ
library(ggrepel)
library(viridis) # viridisカラーパレットを扱うパッケージ。ユニバーサルデザインに必須。
library(viridisLite) # 

# 2. データを読み込む
# データは******からダウンロードしてからRstudio cloudにアップロードする
# もしくは直接読み込む
# 読み込むには、readxlパッケージのread_excel()関数を使う。
tp_01 <- 
  readxl::read_excel(
    path = "total_population_01.xlsx", # ファイルが置いてある場所を指定
    sheet = "Data" # データがあるMSExcelシート
  )

# 3. データを読み込む
# %>% はパイプと呼ばれる関数。
# 左側にあるオブジェクトを、右側にある関数の第一引数に代入する。
# 都度変数をつくり代入する作業を繰り返さずに済むから便利。
tp_01_tidy <- 
  tp_01 %>% 
  # tidyrパッケージのpivot_longer関数を使う。
  # 官庁統計配布時に使われるMSExcelで使いやすい横長データを、
  # Rで使いやすい縦長データに変換する関数
  tidyr::pivot_longer(
    cols = c("total", "female", "male"),
    names_to = "gender",
    values_to = "population"
    ) %>% 
  # もともとの変数名（列名）をまとめるときに使う変数名  # dplyr::mutate()関数
  # オブジェクトに変数を加えるとき、あるいは
  # すでにある変数を変換するときに使う
  dplyr::mutate(
    gender = factor(gender), # 性別を表現する変数を因子型へ変換 
    # lubridateパッケージにあるymd()関数
    # 日付に見える文字列を、日付型データに変換する。
    # 時系列データを取り扱うときはきわめて便利。
    year = lubridate::ymd(
      # baseパッケージにあるpaste()関数。
      # オブジェクトや文字列を連結する関数。
      base::paste(
        year, "-01-01",
        sep = "" # 連結する変数相互に空白を開けない
        )
      )
    )






# 4. 折れ線グラフを作図する
# ggplot2::ggplot()関数が大活躍!!
tp_01_lineplot_01 <- 
  tp_01_tidy %>% 
  #グラフエリアを作る
  ggplot(
    aes(
      x = year, # x軸にはyearと名付けた変数を使う
      y = population, # y軸にはpopulationと名付けた変数を使う
      color = gender # 折れ線グラフ塗り分けにはgenderと名付けた変数を使う
    )
  ) +
  # 折れ線グラフを描く
  geom_line() +
  # データがある各年に点を加える
  geom_point() +
  # 凡例を折れ線グラフ右端にそろえる
  geom_text_repel(
    data = tp_01_tidy %>% 
      dplyr::filter(
        year == max(year)
      ),
    aes(label = gender),
    nudge_x = 50,
    size = 6
  ) +
  # 表題と軸ラベル名を設定する
  labs(
    title = "Population in Asian Countries", # メインタイトル
    subtitle = "Source: UN World Population Prospects (https://population.un.org/wpp/)", # サブタイトル
    x = "Year (1950-2100)", # x軸名
    y = "Population (Unit: 1,000Pax.)", # y軸名 
    color = "Gender" # 凡例名
  ) +
  # scale_color_viridis()関数
  # viridisパッケージを使ってユニバーサルデザインな配色に変える
  scale_color_viridis(
    option = "plasma", # plasmaと名付けられた配色を使う。他にもさまざまな配色がある
    discrete = TRUE # 塗り分けに使う変数は離散的であることを宣言する。
  ) +
  # Classicテーマを使う
  theme_classic() +
  # 見た目詳細を変更する
  theme(
    legend.position = "none", # 凡例位置を調整。値はx軸y軸各々について原点からの距離。
    legend.text = element_text(size = 12), # 
    axis.text = element_text(size = 12) # 
  )

# 5. グラフを保存する
ggsave(
  "tp_01_lineplot_01.pdf", # ファイル名
  plot = tp_01_lineplot_01 # 保存するグラフを格納したオブジェクト
  )

#
##
### おしまい ---

