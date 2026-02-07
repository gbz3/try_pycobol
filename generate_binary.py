#!/usr/bin/env python3
"""
テスト用バイナリデータを生成するスクリプト
parse_binary.py で解析可能なフォーマットでデータを生成
ゾーン10進数（Zoned Decimal）にも対応
"""
import sys
import struct
import argparse
import random

# レコードフォーマット（parse_binary.pyと同じ）
RECORD_FORMAT = '>ifH10s8s'
RECORD_SIZE = struct.calcsize(RECORD_FORMAT)


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


def generate_record(record_num):
    """
    1レコード分のテストデータを生成
    
    Args:
        record_num: レコード番号
    
    Returns:
        bytes: バイナリデータ
    """
    # テストデータを生成
    integer_value = record_num
    float_value = record_num * 1.5
    short_value = record_num % 65536  # unsigned shortの範囲内
    string_value = f"REC{record_num:06d}".encode('utf-8')[:10].ljust(10, b'\x00')
    
    # ゾーン10進数: record_numの10倍（正負を交互に）
    zoned_value = record_num * 10 if record_num % 2 == 0 else -(record_num * 10)
    zoned_bytes = encode_zoned_decimal(zoned_value, length=8)
    
    # バイナリデータにパック
    packed_data = struct.pack(RECORD_FORMAT, integer_value, float_value, short_value, string_value, zoned_bytes)
    
    return packed_data


def generate_random_record(record_num):
    """
    ランダムなテストデータを含む1レコードを生成
    
    Args:
        record_num: レコード番号
    
    Returns:
        bytes: バイナリデータ
    """
    # ランダムなテストデータを生成
    integer_value = random.randint(-2147483648, 2147483647)
    float_value = random.uniform(-1000.0, 1000.0)
    short_value = random.randint(0, 65535)
    
    # ランダムな文字列（英数字）
    chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
    random_str = ''.join(random.choice(chars) for _ in range(random.randint(5, 10)))
    string_value = random_str.encode('utf-8')[:10].ljust(10, b'\x00')
    
    # ランダムなゾーン10進数（-99999999 ～ 99999999）
    zoned_value = random.randint(-99999999, 99999999)
    zoned_bytes = encode_zoned_decimal(zoned_value, length=8)
    
    # バイナリデータにパック
    packed_data = struct.pack(RECORD_FORMAT, integer_value, float_value, short_value, string_value, zoned_bytes)
    
    return packed_data


def generate_binary_data(num_records, output_stream, random_data=False, chunk_size=1000):
    """
    バイナリデータを生成して出力
    
    Args:
        num_records: 生成するレコード数
        output_stream: 出力ストリーム
        random_data: Trueの場合ランダムなデータを生成
        chunk_size: メモリ効率のため、何レコードごとにフラッシュするか
    """
    buffer = b''
    
    for i in range(1, num_records + 1):
        # レコードを生成
        if random_data:
            record = generate_random_record(i)
        else:
            record = generate_record(i)
        
        buffer += record
        
        # チャンク単位で出力（メモリ効率のため）
        if i % chunk_size == 0:
            output_stream.write(buffer)
            buffer = b''
            
            # 進捗表示（標準エラー出力）
            if i % 10000 == 0:
                print(f"生成済み: {i}/{num_records}レコード", file=sys.stderr)
    
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
        print(f"レコード数: {args.num_records}", file=sys.stderr)
        print(f"レコードサイズ: {RECORD_SIZE}バイト", file=sys.stderr)
        print(f"総データサイズ: {args.num_records * RECORD_SIZE:,}バイト ({(args.num_records * RECORD_SIZE) / (1024**2):.2f} MB)", file=sys.stderr)
        print(f"フォーマット: {RECORD_FORMAT}", file=sys.stderr)
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
