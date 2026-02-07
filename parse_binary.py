#!/usr/bin/env python3
"""
標準入力からバイナリデータを段階的に読み取り、structモジュールで解析する例
数ギガバイト規模のデータに対応
ゾーン10進数（Zoned Decimal）にも対応
"""
import sys
import struct

# レコードフォーマット定義
# '>' はビッグエンディアン
# 'i' = int (4バイト)
# 'f' = float (4バイト)
# 'H' = unsigned short (2バイト)
# '10s' = 10バイトの文字列
# '8s' = 8バイトのゾーン10進数
RECORD_FORMAT = '>ifH10s8s'
RECORD_SIZE = struct.calcsize(RECORD_FORMAT)


def decode_zoned_decimal(zoned_bytes):
    """
    ゾーン10進数（Zoned Decimal）をデコード
    
    ゾーン10進数形式:
    - 各バイトの下位4ビット（ニブル）が数字（0-9）
    - 最後のバイトの上位4ビット（ゾーン）が符号
      - 0xC, 0xF: 正の数
      - 0xD: 負の数
    
    Args:
        zoned_bytes: ゾーン10進数のバイト列
    
    Returns:
        int: デコードされた整数値
    
    Raises:
        ValueError: 不正なゾーン10進数フォーマット
    """
    if not zoned_bytes:
        raise ValueError("空のバイト列です")
    
    digits = []
    is_negative = False
    
    for i, byte in enumerate(zoned_bytes):
        # 上位4ビット（ゾーン）と下位4ビット（数字）を分離
        zone = (byte >> 4) & 0x0F
        digit = byte & 0x0F
        
        # 数字部分の検証
        if digit > 9:
            raise ValueError(f"不正な数字: {digit} (バイト位置: {i})")
        
        # 最後のバイト: 符号をチェック
        if i == len(zoned_bytes) - 1:
            if zone == 0xD:
                is_negative = True
            elif zone not in (0xC, 0xF):
                # 警告として処理するが、データは読み取る
                pass
        else:
            # 最後以外のバイト: 通常はゾーンが0xFであることを期待
            if zone != 0xF:
                # 警告として処理するが、データは読み取る
                pass
        
        digits.append(str(digit))
    
    # 数字列を整数に変換
    number_str = ''.join(digits)
    try:
        value = int(number_str)
    except ValueError:
        raise ValueError(f"数字列を整数に変換できません: {number_str}")
    
    return -value if is_negative else value


def parse_record(data):
    """
    1レコード分のバイナリデータを解析してdictに格納
    
    Args:
        data: バイナリデータ (RECORD_SIZE バイト)
    
    Returns:
        dict: 解析結果
    """
    if len(data) < RECORD_SIZE:
        return None
    
    # データを解析
    values = struct.unpack(RECORD_FORMAT, data[:RECORD_SIZE])
    
    # ゾーン10進数をデコード
    try:
        zoned_value = decode_zoned_decimal(values[4])
    except ValueError as e:
        zoned_value = f"ERROR: {e}"
    
    # 結果をdictに格納
    result = {
        'integer_value': values[0],
        'float_value': values[1],
        'short_value': values[2],
        'string_value': values[3].decode('utf-8', errors='ignore').rstrip('\x00'),
        'zoned_decimal': zoned_value,
        'zoned_decimal_hex': values[4].hex(),
    }
    
    return result


def process_stream_chunked(input_stream, chunk_size=1024*1024):
    """
    ストリームから段階的にデータを読み取り、レコード単位で処理
    
    Args:
        input_stream: バイナリ入力ストリーム
        chunk_size: チャンクサイズ (デフォルト: 1MB)
    
    Yields:
        dict: 解析されたレコード
    """
    buffer = b''
    record_count = 0
    
    while True:
        # チャンク単位でデータを読み込む
        chunk = input_stream.read(chunk_size)
        
        if not chunk:
            # 入力終了
            if buffer:
                # バッファに残っているデータを処理
                if len(buffer) >= RECORD_SIZE:
                    print(f"警告: {len(buffer)}バイトの不完全なデータが残っています", file=sys.stderr)
            break
        
        # バッファに追加
        buffer += chunk
        
        # バッファから完全なレコードを取り出して処理
        while len(buffer) >= RECORD_SIZE:
            record_data = buffer[:RECORD_SIZE]
            buffer = buffer[RECORD_SIZE:]
            
            parsed = parse_record(record_data)
            if parsed:
                record_count += 1
                parsed['record_number'] = record_count
                yield parsed


def main():
    """
    メイン処理: 標準入力から段階的にバイナリデータを読み取り解析
    """
    try:
        print("=== レコード解析開始 ===", file=sys.stderr)
        print(f"レコードサイズ: {RECORD_SIZE}バイト", file=sys.stderr)
        print(f"フォーマット: {RECORD_FORMAT}", file=sys.stderr)
        print("", file=sys.stderr)
        
        record_count = 0
        
        # 段階的に処理（1MBチャンクで読み込み）
        for parsed_data in process_stream_chunked(sys.stdin.buffer, chunk_size=1024*1024):
            record_count += 1
            
            # 結果を出力
            print(parsed_data)
            
            # 進捗表示（10000レコードごと）
            if record_count % 10000 == 0:
                print(f"処理済み: {record_count}レコード", file=sys.stderr)
        
        print("", file=sys.stderr)
        print(f"=== 解析完了 ===", file=sys.stderr)
        print(f"総レコード数: {record_count}", file=sys.stderr)
        
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
