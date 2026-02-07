#!/usr/bin/env python3
"""
テスト用バイナリデータを生成するスクリプト
parse_binary.py で解析可能なフォーマットでデータを生成
ゾーン10進数（Zoned Decimal）にも対応
レコード区分による複数フォーマットに対応
"""
import sys
import struct
import argparse
import random
from datetime import datetime

# レコード区分（ASCII文字）
RECORD_TYPE_HEADER = b'1'
RECORD_TYPE_DATA = b'2'
RECORD_TYPE_TRAILER = b'8'
RECORD_TYPE_END = b'9'

# 各レコードタイプのフォーマット定義（parse_binary.pyと同じ）
HEADER_FORMAT = '>c8s10s50s'
DATA_FORMAT = '>cifH10s8s'
TRAILER_FORMAT = '>cI8s56s'
END_FORMAT = '>cB67s'


def encode_zoned_decimal(value, length=8):
    """
    整数値をゾーン10進数（Zoned Decimal）にエンコード
    
    ゾーン10進数形式:
    - 各バイトの下位4ビット（ニブル）が数字（0-9）
    - 最後のバイトの上位4ビット（ゾーン）が符号
      - 0xC: 正の数
      - 0xD: 負の数
      - 0xF: 通常のゾーン（符号なし）
    
    Args:
        value: エンコードする整数値
        length: バイト長（デフォルト: 8）
    
    Returns:
        bytes: ゾーン10進数のバイト列
    
    Raises:
        ValueError: 値がバイト長に収まらない場合
    """
    # 符号を判定
    is_negative = value < 0
    abs_value = abs(value)
    
    # 数字列に変換（ゼロ埋め）
    digits_str = str(abs_value).zfill(length)
    
    if len(digits_str) > length:
        raise ValueError(f"値 {value} は {length} バイトに収まりません")
    
    # ゾーン10進数バイト列を生成
    zoned_bytes = bytearray()
    
    for i, digit_char in enumerate(digits_str):
        digit = int(digit_char)
        
        # 最後のバイト: 符号ゾーンを設定
        if i == len(digits_str) - 1:
            if is_negative:
                zone = 0xD  # 負の数
            else:
                zone = 0xC  # 正の数
        else:
            zone = 0xF  # 通常のゾーン
        
        # ゾーン（上位4ビット）と数字（下位4ビット）を結合
        byte = (zone << 4) | digit
        zoned_bytes.append(byte)
    
    return bytes(zoned_bytes)


def generate_header_record():
    """ヘッダレコードを生成"""
    record_type = RECORD_TYPE_HEADER
    created_date = datetime.now().strftime('%Y%m%d').encode('cp932')
    system_id = 'TESTSYS001'.encode('cp932').ljust(10, b'\x00')
    reserved = b'\x00' * 50
    
    return struct.pack(HEADER_FORMAT, record_type, created_date, system_id, reserved)


def generate_data_record(record_num, random_data=False):
    """
    データレコードを生成
    
    Args:
        record_num: レコード番号
        random_data: ランダムデータを生成するか
    
    Returns:
        bytes: バイナリデータ
    """
    record_type = RECORD_TYPE_DATA
    
    if random_data:
        integer_value = random.randint(-2147483648, 2147483647)
        float_value = random.uniform(-1000.0, 1000.0)
        short_value = random.randint(0, 65535)
        # 半角カナを含むランダム文字列
        kana_chars = 'ｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜｦﾝ'
        ascii_chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
        # 半角カナと英数字を混ぜる
        if random.random() < 0.5:
            random_str = ''.join(random.choice(kana_chars) for _ in range(random.randint(3, 6)))
        else:
            random_str = ''.join(random.choice(ascii_chars) for _ in range(random.randint(5, 10)))
        string_value = random_str.encode('cp932')[:10].ljust(10, b'\x00')
        zoned_value = random.randint(-99999999, 99999999)
    else:
        integer_value = record_num
        float_value = record_num * 1.5
        short_value = record_num % 65536
        # レコード番号によって半角カナと英数字を使い分け
        if record_num % 3 == 0:
            # 半角カナのテストデータ
            kana_samples = ['ﾃｽﾄ', 'ｻﾝﾌﾟﾙ', 'ﾃﾞｰﾀ', 'ｶﾅ', 'ﾚｺｰﾄﾞ']
            kana_str = kana_samples[(record_num // 3) % len(kana_samples)] + f"{record_num:02d}"
            string_value = kana_str.encode('cp932')[:10].ljust(10, b'\x00')
        else:
            string_value = f"REC{record_num:06d}".encode('cp932')[:10].ljust(10, b'\x00')
        zoned_value = record_num * 10 if record_num % 2 == 0 else -(record_num * 10)
    
    zoned_bytes = encode_zoned_decimal(zoned_value, length=8)
    
    return struct.pack(DATA_FORMAT, record_type, integer_value, float_value, short_value, string_value, zoned_bytes)


def generate_trailer_record(record_count, total_amount):
    """トレーラレコードを生成"""
    record_type = RECORD_TYPE_TRAILER
    amount_zoned = encode_zoned_decimal(total_amount, length=8)
    reserved = b'\x00' * 56
    
    return struct.pack(TRAILER_FORMAT, record_type, record_count, amount_zoned, reserved)


def generate_end_record():
    """エンドレコードを生成"""
    record_type = RECORD_TYPE_END
    end_code = 0x00  # 正常終了
    reserved = b'\x00' * 67
    
    return struct.pack(END_FORMAT, record_type, end_code, reserved)





def generate_binary_data(num_data_records, output_stream, random_data=False, chunk_size=1000):
    """
    バイナリデータを生成して出力（ヘッダ、データ、トレーラ、エンドの完全なファイル）
    
    Args:
        num_data_records: 生成するデータレコード数
        output_stream: 出力ストリーム
        random_data: Trueの場合ランダムなデータを生成
        chunk_size: メモリ効率のため、何レコードごとにフラッシュするか
    """
    buffer = b''
    total_amount = 0
    
    # 1. ヘッダレコード
    buffer += generate_header_record()
    
    # 2. データレコード
    for i in range(1, num_data_records + 1):
        record = generate_data_record(i, random_data)
        buffer += record
        
        # 合計金額を計算（簡易的にレコード番号の10倍を加算）
        if random_data:
            total_amount += random.randint(-99999999, 99999999)
        else:
            total_amount += i * 10 if i % 2 == 0 else -(i * 10)
        
        # チャンク単位で出力（メモリ効率のため）
        if i % chunk_size == 0:
            output_stream.write(buffer)
            buffer = b''
            
            # 進捗表示（標準エラー出力）
            if i % 10000 == 0:
                print(f"生成済み: {i}/{num_data_records}データレコード", file=sys.stderr)
    
    # 3. トレーラレコード
    buffer += generate_trailer_record(num_data_records, total_amount)
    
    # 4. エンドレコード
    buffer += generate_end_record()
    
    # 残りのバッファを出力
    if buffer:
        output_stream.write(buffer)
    
    output_stream.flush()


def main():
    parser = argparse.ArgumentParser(
        description='テスト用バイナリデータを生成',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
使用例:
  # 100レコードを生成して test.bin に保存
  %(prog)s -n 100 > test.bin
  
  # 100万レコードをランダムデータで生成
  %(prog)s -n 1000000 --random > large_test.bin
  
  # 生成したデータをパイプで直接解析
  %(prog)s -n 1000 | python parse_binary.py
  
  # 大容量データ（1GB相当）を生成
  %(prog)s -n 50000000 > large.bin
        """
    )
    
    parser.add_argument(
        '-n', '--num-records',
        type=int,
        default=10,
        help='生成するレコード数 (デフォルト: 10)'
    )
    
    parser.add_argument(
        '--random',
        action='store_true',
        help='ランダムなデータを生成'
    )
    
    parser.add_argument(
        '--seed',
        type=int,
        default=None,
        help='乱数シード（再現性のため）'
    )
    
    args = parser.parse_args()
    
    # 乱数シード設定
    if args.seed is not None:
        random.seed(args.seed)
    
    try:
        print(f"=== バイナリデータ生成開始 ===", file=sys.stderr)
        print(f"データレコード数: {args.num_records}", file=sys.stderr)
        print(f"総レコード数: {args.num_records + 3} (ヘッダ1 + データ{args.num_records} + トレーラ1 + エンド1)", file=sys.stderr)
        print(f"ランダムデータ: {'有効' if args.random else '無効'}", file=sys.stderr)
        print("", file=sys.stderr)
        
        # バイナリデータを生成
        generate_binary_data(args.num_records, sys.stdout.buffer, args.random)
        
        print("", file=sys.stderr)
        print("=== 生成完了 ===", file=sys.stderr)
        
        return 0
        
    except KeyboardInterrupt:
        print("\n処理が中断されました", file=sys.stderr)
        return 130
    except Exception as e:
        print(f"エラー: {e}", file=sys.stderr)
        import traceback
        traceback.print_exc(file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
