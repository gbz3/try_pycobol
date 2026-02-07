#!/usr/bin/env python3
"""
標準入力からバイナリデータを段階的に読み取り、structモジュールで解析する例
数ギガバイト規模のデータに対応
ゾーン10進数（Zoned Decimal）にも対応
レコード区分による複数フォーマットに対応
"""
import sys
import struct
import json

# レコード区分（ASCII文字）
RECORD_TYPE_HEADER = b'1'
RECORD_TYPE_DATA = b'2'
RECORD_TYPE_TRAILER = b'8'
RECORD_TYPE_END = b'9'

# 各レコードタイプのフォーマット定義（先頭1バイトはレコード区分）
# ヘッダレコード: 区分(1c) + 作成日(8s) + システムID(10s) + 予備(50s)
HEADER_FORMAT = '>c8s10s50s'
HEADER_SIZE = struct.calcsize(HEADER_FORMAT)

# データレコード: 区分(1c) + int(4) + float(4) + short(2) + 文字列(10s) + ゾーン10進数(8s)
DATA_FORMAT = '>cifH10s8s'
DATA_SIZE = struct.calcsize(DATA_FORMAT)

# トレーラレコード: 区分(1c) + レコード件数(4) + 合計金額(8s:ゾーン10進数) + 予備(56s)
TRAILER_FORMAT = '>cI8s56s'
TRAILER_SIZE = struct.calcsize(TRAILER_FORMAT)

# エンドレコード: 区分(1c) + 終了コード(1) + 予備(67s)
END_FORMAT = '>cB67s'
END_SIZE = struct.calcsize(END_FORMAT)

# 最大レコードサイズ（バッファリング用）
MAX_RECORD_SIZE = max(HEADER_SIZE, DATA_SIZE, TRAILER_SIZE, END_SIZE)


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


def parse_header_record(data):
    """ヘッダレコードを解析"""
    values = struct.unpack(HEADER_FORMAT, data[:HEADER_SIZE])
    return {
        'record_type': 'HEADER',
        'record_type_code': values[0].decode('ascii'),
        'created_date': values[1].decode('cp932', errors='ignore').rstrip('\x00'),
        'system_id': values[2].decode('cp932', errors='ignore').rstrip('\x00'),
        'reserved': values[3].hex()[:20] + '...',  # 最初の10バイト分のみ表示
    }, HEADER_SIZE


def parse_data_record(data):
    """データレコードを解析"""
    values = struct.unpack(DATA_FORMAT, data[:DATA_SIZE])
    
    # ゾーン10進数をデコード
    try:
        zoned_value = decode_zoned_decimal(values[5])
    except ValueError as e:
        zoned_value = f"ERROR: {e}"
    
    return {
        'record_type': 'DATA',
        'record_type_code': values[0].decode('ascii'),
        'integer_value': values[1],
        'float_value': values[2],
        'short_value': values[3],
        'string_value': values[4].decode('cp932', errors='ignore').rstrip('\x00'),
        'zoned_decimal': zoned_value,
        'zoned_decimal_hex': values[5].hex(),
    }, DATA_SIZE


def parse_trailer_record(data):
    """トレーラレコードを解析"""
    values = struct.unpack(TRAILER_FORMAT, data[:TRAILER_SIZE])
    
    # ゾーン10進数をデコード
    try:
        total_amount = decode_zoned_decimal(values[2])
    except ValueError as e:
        total_amount = f"ERROR: {e}"
    
    return {
        'record_type': 'TRAILER',
        'record_type_code': values[0].decode('ascii'),
        'record_count': values[1],
        'total_amount': total_amount,
        'total_amount_hex': values[2].hex(),
        'reserved': values[3].hex()[:20] + '...',
    }, TRAILER_SIZE


def parse_end_record(data):
    """エンドレコードを解析"""
    values = struct.unpack(END_FORMAT, data[:END_SIZE])
    return {
        'record_type': 'END',
        'record_type_code': values[0].decode('ascii'),
        'end_code': values[1],
        'reserved': values[2].hex()[:20] + '...',
    }, END_SIZE


def parse_record(data):
    """
    1レコード分のバイナリデータを解析してdictに格納
    レコード区分に応じて適切なパーサーを呼び出す
    
    Args:
        data: バイナリデータ
    
    Returns:
        tuple: (解析結果のdict, 消費したバイト数) または (None, 0)
    """
    if len(data) < 1:
        return None, 0
    
    # レコード区分を読み取る（先頭1バイト、ASCII文字）
    record_type = bytes([data[0]])
    
    # レコードタイプに応じて適切なパーサーを呼び出す
    try:
        if record_type == RECORD_TYPE_HEADER:
            if len(data) < HEADER_SIZE:
                return None, 0
            return parse_header_record(data)
        elif record_type == RECORD_TYPE_DATA:
            if len(data) < DATA_SIZE:
                return None, 0
            return parse_data_record(data)
        elif record_type == RECORD_TYPE_TRAILER:
            if len(data) < TRAILER_SIZE:
                return None, 0
            return parse_trailer_record(data)
        elif record_type == RECORD_TYPE_END:
            if len(data) < END_SIZE:
                return None, 0
            return parse_end_record(data)
        else:
            print(f"警告: 未知のレコード区分: {record_type} (ASCII: '{record_type.decode('ascii', errors='replace')}')", file=sys.stderr)
            return {
                'record_type': 'UNKNOWN',
                'record_type_code': record_type,
                'error': f'未知のレコード区分: {record_type}',
            }, 1  # 1バイトだけスキップ
    except Exception as e:
        return {
            'record_type': 'ERROR',
            'record_type_code': record_type,
            'error': str(e),
        }, 1


def process_stream_chunked(input_stream, chunk_size=1024*1024):
    """
    ストリームから段階的にデータを読み取り、レコード単位で処理
    レコード区分に応じて可変長レコードを処理
    
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
                if len(buffer) > 0:
                    print(f"警告: {len(buffer)}バイトの不完全なデータが残っています", file=sys.stderr)
                    print(f"残データ(hex): {buffer.hex()}", file=sys.stderr)
            break
        
        # バッファに追加
        buffer += chunk
        
        # バッファから完全なレコードを取り出して処理
        while len(buffer) > 0:
            # レコードを解析（可変長）
            parsed, consumed = parse_record(buffer)
            
            if consumed == 0:
                # データ不足で解析できない場合は次のチャンクを待つ
                break
            
            # バッファから消費した分を削除
            buffer = buffer[consumed:]
            
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
        print(f"ヘッダレコードサイズ: {HEADER_SIZE}バイト", file=sys.stderr)
        print(f"データレコードサイズ: {DATA_SIZE}バイト", file=sys.stderr)
        print(f"トレーラレコードサイズ: {TRAILER_SIZE}バイト", file=sys.stderr)
        print(f"エンドレコードサイズ: {END_SIZE}バイト", file=sys.stderr)
        print("", file=sys.stderr)
        
        record_count = 0
        
        # 段階的に処理（1MBチャンクで読み込み）
        for parsed_data in process_stream_chunked(sys.stdin.buffer, chunk_size=1024*1024):
            record_count += 1
            
            # 結果をJSON形式で出力
            print(json.dumps(parsed_data, ensure_ascii=False, indent=2))
            
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
