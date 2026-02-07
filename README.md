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
(.venv) $ python generate_binary.py -n 5 |hexdump -C -v
=== バイナリデータ生成開始 ===
データレコード数: 5
総レコード数: 8 (ヘッダ1 + データ5 + トレーラ1 + エンド1)
ランダムデータ: 無効


=== 生成完了 ===
00000000  31 32 30 32 36 30 32 30  37 54 45 53 54 53 59 53  |120260207TESTSYS|
00000010  30 30 31 00 00 00 00 00  00 00 00 00 00 00 00 00  |001.............|
00000020  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|
00000030  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|
00000040  00 00 00 00 00 32 00 00  00 01 3f c0 00 00 00 01  |.....2....?.....|
00000050  52 45 43 30 30 30 30 30  31 00 f0 f0 f0 f0 f0 f0  |REC000001.......|
00000060  f1 d0 32 00 00 00 02 40  40 00 00 00 02 52 45 43  |..2....@@....REC|
00000070  30 30 30 30 30 32 00 f0  f0 f0 f0 f0 f0 f2 c0 32  |000002.........2|
00000080  00 00 00 03 40 90 00 00  00 03 bb dd cc df d9 30  |....@..........0|
00000090  33 00 00 00 f0 f0 f0 f0  f0 f0 f3 d0 32 00 00 00  |3...........2...|
000000a0  04 40 c0 00 00 00 04 52  45 43 30 30 30 30 30 34  |.@.....REC000004|
000000b0  00 f0 f0 f0 f0 f0 f0 f4  c0 32 00 00 00 05 40 f0  |.........2....@.|
000000c0  00 00 00 05 52 45 43 30  30 30 30 30 35 00 f0 f0  |....REC000005...|
000000d0  f0 f0 f0 f0 f5 d0 38 00  00 00 05 f0 f0 f0 f0 f0  |......8.........|
000000e0  f0 f3 d0 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|
000000f0  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|
00000100  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|
00000110  00 00 00 00 00 00 00 00  00 00 00 39 00 00 00 00  |...........9....|
00000120  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|
00000130  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|
00000140  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|
00000150  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|
00000160
```

```
# 解析結果
(.venv) $ python generate_binary.py -n 5 |python parse_binary.py 
=== レコード解析開始 ===
ヘッダレコードサイズ: 69バイト
データレコードサイズ: 29バイト
トレーラレコードサイズ: 69バイト
エンドレコードサイズ: 69バイト

=== バイナリデータ生成開始 ===
データレコード数: 5
総レコード数: 8 (ヘッダ1 + データ5 + トレーラ1 + エンド1)
ランダムデータ: 無効


=== 生成完了 ===
{'record_type': 'HEADER', 'record_type_code': '1', 'created_date': '20260207', 'system_id': 'TESTSYS001', 'reserved': '00000000000000000000...', 'record_number': 1}
{'record_type': 'DATA', 'record_type_code': '2', 'integer_value': 1, 'float_value': 1.5, 'short_value': 1, 'string_value': 'REC000001', 'zoned_decimal': -10, 'zoned_decimal_hex': 'f0f0f0f0f0f0f1d0', 'record_number': 2}
{'record_type': 'DATA', 'record_type_code': '2', 'integer_value': 2, 'float_value': 3.0, 'short_value': 2, 'string_value': 'REC000002', 'zoned_decimal': 20, 'zoned_decimal_hex': 'f0f0f0f0f0f0f2c0', 'record_number': 3}
{'record_type': 'DATA', 'record_type_code': '2', 'integer_value': 3, 'float_value': 4.5, 'short_value': 3, 'string_value': 'ｻﾝﾌﾟﾙ03', 'zoned_decimal': -30, 'zoned_decimal_hex': 'f0f0f0f0f0f0f3d0', 'record_number': 4}
{'record_type': 'DATA', 'record_type_code': '2', 'integer_value': 4, 'float_value': 6.0, 'short_value': 4, 'string_value': 'REC000004', 'zoned_decimal': 40, 'zoned_decimal_hex': 'f0f0f0f0f0f0f4c0', 'record_number': 5}
{'record_type': 'DATA', 'record_type_code': '2', 'integer_value': 5, 'float_value': 7.5, 'short_value': 5, 'string_value': 'REC000005', 'zoned_decimal': -50, 'zoned_decimal_hex': 'f0f0f0f0f0f0f5d0', 'record_number': 6}
{'record_type': 'TRAILER', 'record_type_code': '8', 'record_count': 5, 'total_amount': -30, 'total_amount_hex': 'f0f0f0f0f0f0f3d0', 'reserved': '00000000000000000000...', 'record_number': 7}
{'record_type': 'END', 'record_type_code': '9', 'end_code': 0, 'reserved': '00000000000000000000...', 'record_number': 8}

=== 解析完了 ===
総レコード数: 8
```
