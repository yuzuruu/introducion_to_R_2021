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
library(patchwork)
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
  # dplyr::mutate()関数
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

# おまけ tidyなデータとそうでもないデータ
# tidyでもなんでもないただのアヤメのデータ
iris
# おまけ tidyなデータとそうでもないデータ
# アヤメのデータをtidyにする
iris %>% 
  tidyr::pivot_longer(
    cols = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
    names_to = "attributes",
    values_to = "size"
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

# おまけ viridis()パッケージ色比較用グラフ
# データを整形する
iris_tidy <- 
  iris %>% 
  tidyr::pivot_longer(
    cols = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
    names_to = "attributes",
    values_to = "size"
  )
# 密度プロットを描画する
iris.density <- 
  iris_tidy %>% 
  dplyr::filter(attributes == "Sepal.Length") %>% 
  ggplot(aes(x = size, y = ..density.., fill = Species)) +
  geom_density(colour = "transparent", alpha = 0.7) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 12),
    legend.position = "none",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12)
  )
# カラーパレット毎に密度プロットを描く
iris_magma <- 
  iris.density + 
  scale_fill_viridis(
    discrete = TRUE, 
    option = "magma"
  ) + 
  labs(
    title = "magma"
  )
# infernoパレット
iris_inferno <- 
  iris.density + 
  scale_fill_viridis(
    discrete = TRUE, 
    option = "inferno"
  ) + 
  labs(
    title = "inferno"
  )
# magmaパレット
iris_plasma <- 
  iris.density + 
  scale_fill_viridis(
    discrete = TRUE, 
    option = "plasma"
  ) + 
  labs(
    title = "plasma"
  )
# viridisパレット
iris_viridis <- 
  iris.density + 
  scale_fill_viridis(
    discrete = TRUE, 
    option = "viridis"
  ) + 
  labs(
    title = "Viridis"
  )
# cividisパレット
iris_cividis <- 
  iris.density + 
  scale_fill_viridis(
    discrete = TRUE, 
    option = "cividis"
  ) + 
  labs(
    title = "cividis"
  )
# 密度プロットをならべる
iris_density_comparison <- 
  iris_magma + iris_inferno + iris_plasma + iris_viridis + iris_cividis
ggsave("./img/iris_density_comparison.png", plot = iris_density_comparison)

#
##
### おしまい ---


# おまけ　相関係数いろいろ
# 任意の相関係数を有する正規乱数を発生させる関数
# mu:平均
# sigma:標準偏差
# rho:相関係数
# n:発生させる乱数の個数
# ここからいただきました
# http://cse.naro.affrc.go.jp/takezawa/r-tips/r/60.html
r2norm <- 
  function(n, mu, sigma, rho) {
    tmp <- rnorm(n)
    x   <- mu+sigma*tmp
    y   <- rho*x + sqrt(1-rho^2)*rnorm(n)
    return(
      data.frame(
        x=x,
        y=y
        )
      )
}
# 正規乱数を発生させる
# r = 0.05
datap0.05 <- 
  r2norm(1000, 0, 1, 0.05) %>% 
  mutate(attribute = factor("r=0.05"))
# r = 0.5
datap0.5 <- 
  r2norm(1000, 0, 1, 0.5) %>% 
  mutate(attribute = factor("r=0.5"))
# r = 0.95
datap0.95 <- 
  r2norm(1000, 0, 1, 0.95) %>% 
  mutate(attribute = factor("r=0.95"))
# r = -0.05
datam0.05 <- 
  r2norm(1000, 0, 1, -0.05) %>% 
  mutate(attribute = factor("r=-0.05")) 
# r = -0.5
datam0.5 <- 
  r2norm(1000, 0, 1, -0.5) %>% 
  mutate(attribute = factor("r=-0.5")) 
# r = -0.95
datam0.95 <- 
  r2norm(1000, 0, 1, -0.95) %>% 
  mutate(attribute = factor("r=-0.95"))
# データを結合してtidy化して作図
correl_comparison <- 
  list(datap0.05, datap0.5, datap0.95, datam0.05, datam0.5, datam0.95) %>% 
  do.call(rbind, .) %>% 
  as_tibble() %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  facet_wrap(~ attribute) +
  theme_classic() +
  theme(
    strip.background = element_blank()
  )

# 相関係数を考えるにはまずモーメントから
# 相関係数0.9な乱数を1,000つ発生させる
data <- r2norm(1000, 0, 1, 0.9)
# モーメントを考える模式的な散布図を作図する
sp_01 <- 
  data %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point(color = "lightblue") +
  geom_segment(x =0,y=-2,xend=0,yend=2)+
  geom_segment(x =-2,y=0,xend=2,yend=0)+
  geom_segment(aes(x =0,y=1,xend=0.8,yend=1), size = 1.2, arrow = arrow(length = unit(0.2, "cm")))+
  geom_segment(aes(x =0.8,y=1,xend=0,yend=1), size = 1.2, arrow = arrow(length = unit(0.2, "cm")))+
  geom_segment(aes(x =0.8,y=0,xend=0.8,yend=1), size = 1.2, arrow = arrow(length = unit(0.2, "cm")))+
  geom_segment(aes(x =0.8,y=1,xend=0.8,yend=0), size = 1.2, arrow = arrow(length = unit(0.2, "cm")))+
  annotate("text", x = 0, y = -2, label = "m[x]", parse = TRUE, size = 6)+
  annotate("text", x = -2, y = 0, label = "m[y]", parse = TRUE, size = 6)+
  annotate("text", x = 0.5, y = 1.2, label = "x[i]-m[x]", parse = TRUE, size = 8)+
  annotate("text", x = 1.0, y = 0.3, label = "y[i]-m[y]", parse = TRUE, size = 8)+
  xlim(-2,2) +
  ylim(-2,2) +
  labs(x = "x", y = "y") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)
  )
sp_01

#
##
### おしまい ---
