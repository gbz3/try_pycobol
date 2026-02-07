# try_pycobol

## 開発フロー

```
# 仮想環境の作成
python -m venv .venv

# 仮想環境の有効化
source .venv/bin/activate

# パッケージのインストール
pip install <必要なパッケージ>

# pip upgrade
pip install --upgrade pip
```

## パッケージ

```
(.venv) $ pip install cobx
ERROR: Could not find a version that satisfies the requirement cobx (from versions: none)
ERROR: No matching distribution found for cobx
```

```
3. なぜパッケージが見つからないのか？
COBOL の Copybook 解析は、以下の理由で「これ一つでOK」という決定版ライブラリが育ちにくい背景があります。

方言が多い: IBM, Fujitsu, NEC などのベンダーごとに COPY 句の挙動が微妙に違う。

エンコード: EBCDIC なのか UTF-8 なのか、データの扱いが環境依存。

そのため、多くのプロジェクトでは 「ANTLR4」 というパーサージェネレーターを使って、Python 用の解析器を自作（あるいは GitHub のテンプレートを流用）する構成をとっています。
```

## struct モジュールを使う

### COBOL 定義と Python の対応表

まずは、COBOL の PIC 句を struct の書式文字（Format Characters）にマッピングします。メインフレーム系データは通常 ビッグエンディアン (>) です。

| COBOL 定義 | バイト数 | struct 形式 | 備考 |
|----------|----------|----------|---------|
| PIC X(10)    | 10    | 10s    | 文字列 |
| PIC 9(4) BINARY (COMP)    | 2    | h    | 短精度整数 (short) |
| PIC 9(9) BINARY (COMP)    | 4    | i    | 長精度整数 (int) |
| PIC 9(18) BINARY (COMP)    | 8    | q    | 64bit整数 (long long) |

### 実装例

```
# 生成データ
(.venv) $ python generate_binary.py -n 5 |hexdump -C
=== バイナリデータ生成開始 ===
レコード数: 5
レコードサイズ: 20バイト
総データサイズ: 100バイト (0.00 MB)
フォーマット: >ifH10s
ランダムデータ: 無効


=== 生成完了 ===
00000000  00 00 00 01 3f c0 00 00  00 01 52 45 43 30 30 30  |....?.....REC000|
00000010  30 30 31 00 00 00 00 02  40 40 00 00 00 02 52 45  |001.....@@....RE|
00000020  43 30 30 30 30 30 32 00  00 00 00 03 40 90 00 00  |C000002.....@...|
00000030  00 03 52 45 43 30 30 30  30 30 33 00 00 00 00 04  |..REC000003.....|
00000040  40 c0 00 00 00 04 52 45  43 30 30 30 30 30 34 00  |@.....REC000004.|
00000050  00 00 00 05 40 f0 00 00  00 05 52 45 43 30 30 30  |....@.....REC000|
00000060  30 30 35 00                                       |005.|
00000064
```

```
# 解析結果
(.venv) $ python generate_binary.py -n 5 |python parse_binary.py 
=== レコード解析開始 ===
レコードサイズ: 20バイト
フォーマット: >ifH10s

=== バイナリデータ生成開始 ===
レコード数: 5
レコードサイズ: 20バイト
総データサイズ: 100バイト (0.00 MB)
フォーマット: >ifH10s
ランダムデータ: 無効


=== 生成完了 ===
{'integer_value': 1, 'float_value': 1.5, 'short_value': 1, 'string_value': 'REC000001', 'record_number': 1}
{'integer_value': 2, 'float_value': 3.0, 'short_value': 2, 'string_value': 'REC000002', 'record_number': 2}
{'integer_value': 3, 'float_value': 4.5, 'short_value': 3, 'string_value': 'REC000003', 'record_number': 3}
{'integer_value': 4, 'float_value': 6.0, 'short_value': 4, 'string_value': 'REC000004', 'record_number': 4}
{'integer_value': 5, 'float_value': 7.5, 'short_value': 5, 'string_value': 'REC000005', 'record_number': 5}

=== 解析完了 ===
総レコード数: 5
```
