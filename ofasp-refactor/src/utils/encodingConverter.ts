/**
 * OpenASP Encoding Converter
 * Advanced character code page conversion system
 * Supports multiple encoding types and conversion modes
 */

export interface ConversionMap {
  singleByte: Uint8Array;  // 256 bytes for single-byte conversion
  doubleByte: Uint16Array; // 65536 16-bit values for double-byte conversion
  byteType: Uint8Array;    // 256 bytes indicating single(0) or double(1) byte
  convertType: number;     // Conversion direction
}

export interface ConversionOptions {
  mode: ConversionMode;
  encoding: EncodingType;
  useSOSI: boolean;
  sosiType?: '0E0F' | '1E1F' | 'custom';
  customSO?: string;
  customSI?: string;
  sosiHandling?: 'remove' | 'keep' | 'space';  // SOSI 코드 처리 방식
  errorHandling: ErrorHandling;
}

export enum ConversionMode {
  EBCDIC_TO_ASCII = 1,
  ASCII_TO_EBCDIC = 2
}

export enum EncodingType {
  US = 'US',
  JP = 'JP', 
  KR = 'KR',
  JAK = 'JAK',
  KEIS = 'KEIS'
}

export enum ErrorHandling {
  STRICT = 'strict',
  REPLACE = 'replace',
  IGNORE = 'ignore'
}

class EncodingConverter {
  private conversionMaps: Map<string, ConversionMap> = new Map();
  private initialized = false;

  constructor() {
    this.initializeConverters();
  }

  private async initializeConverters() {
    if (this.initialized) return;

    // Initialize conversion maps for each encoding type
    await Promise.all([
      this.loadConversionMap(EncodingType.US),
      this.loadConversionMap(EncodingType.JP),
      this.loadConversionMap(EncodingType.KR),
      this.loadConversionMap(EncodingType.JAK),
      this.loadConversionMap(EncodingType.KEIS)
    ]);

    this.initialized = true;
  }

  private async loadConversionMap(encoding: EncodingType): Promise<void> {
    // Create conversion maps using code page files
    const ebcdicToAsciiMap = await this.createEBCDICToASCIIMap(encoding);
    const asciiToEbcdicMap = await this.createASCIIToEBCDICMap(encoding);

    this.conversionMaps.set(`${encoding}_EBC_TO_ASC`, ebcdicToAsciiMap);
    this.conversionMaps.set(`${encoding}_ASC_TO_EBC`, asciiToEbcdicMap);
  }

  private async createEBCDICToASCIIMap(encoding: EncodingType): Promise<ConversionMap> {
    const byteType = new Uint8Array(256);
    
    // Load code page table from file
    const filePath = this.getCodePageFilePath(encoding, ConversionMode.EBCDIC_TO_ASCII);
    const { singleByte, doubleByte } = await this.loadCodePageTable(filePath);
    
    // All bytes are treated as single-byte by default
    // Double-byte processing only happens within SOSI blocks
    for (let i = 0; i < 256; i++) {
      byteType[i] = 0; // Single byte by default
    }

    return {
      singleByte,
      doubleByte,
      byteType,
      convertType: ConversionMode.EBCDIC_TO_ASCII
    };
  }

  private async createASCIIToEBCDICMap(encoding: EncodingType): Promise<ConversionMap> {
    const byteType = new Uint8Array(256);
    
    // Load code page table from file
    const filePath = this.getCodePageFilePath(encoding, ConversionMode.ASCII_TO_EBCDIC);
    const { singleByte, doubleByte } = await this.loadCodePageTable(filePath);
    
    // All bytes are treated as single-byte by default
    // Double-byte processing only happens within SOSI blocks
    for (let i = 0; i < 256; i++) {
      byteType[i] = 0; // Single byte by default
    }

    return {
      singleByte,
      doubleByte,
      byteType,
      convertType: ConversionMode.ASCII_TO_EBCDIC
    };
  }

  private async loadCodePageTable(filePath: string): Promise<{ singleByte: Uint8Array; doubleByte: Uint16Array }> {
    // Load code page table from file
    const singleByte = new Uint8Array(256);
    const doubleByte = new Uint16Array(65536); // Changed to Uint16Array to handle values > 255
    
    try {
      console.log(`Loading code page table from: ${filePath}`);
      const response = await fetch(filePath);
      
      if (!response.ok) {
        throw new Error(`HTTP ${response.status}: ${response.statusText}`);
      }
      
      const content = await response.text();
      const lines = content.split('\n');
      
      let isDoubleByte = false;
      let singleByteCount = 0;
      let doubleByteCount = 0;
      
      for (const line of lines) {
        const trimmed = line.trim();
        
        if (trimmed === '[Double byte mapping table]') {
          isDoubleByte = true;
          console.log(`Found double byte mapping section`);
          continue;
        }
        
        if (trimmed === '[Single byte mapping table]') {
          isDoubleByte = false;
          console.log(`Found single byte mapping section`);
          continue;
        }
        
        // Parse mapping lines like "00 - 00" or "4040 - 8140"
        const match = trimmed.match(/^([0-9A-Fa-f]+)\s*-\s*([0-9A-Fa-f]+)$/);
        if (match) {
          const from = parseInt(match[1], 16);
          const to = parseInt(match[2], 16);
          
          if (isDoubleByte) {
            if (from < 65536 && to !== 0) {
              doubleByte[from] = to;
              doubleByteCount++;
              // Debug specific mapping
              if (from === 0x4040) {
                console.log(`Found 0x4040 mapping: from=0x${from.toString(16).toUpperCase()}, to=0x${to.toString(16).toUpperCase()}`);
                console.log(`Setting doubleByte[${from}] = ${to}`);
                console.log(`Verification: doubleByte[${from}] = ${doubleByte[from]}`);
              }
            }
          } else {
            if (from < 256) {
              singleByte[from] = to;
              singleByteCount++;
              // Debug single byte mapping that might conflict
              if (from === 0x40) {
                console.log(`Found single byte 0x40 mapping: 0x${to.toString(16).toUpperCase()}`);
                console.log(`Setting singleByte[${from}] = ${to}`);
              }
            }
          }
        }
      }
      
      console.log(`Loaded ${singleByteCount} single byte mappings and ${doubleByteCount} double byte mappings`);
      
      // Debug: Check 0x4040 mapping after loading
      if (doubleByte[0x4040] !== 0) {
        console.log(`After loading, doubleByte[0x4040] = ${doubleByte[0x4040]} (0x${doubleByte[0x4040].toString(16).toUpperCase()})`);
      }
      
      return { singleByte, doubleByte };
    } catch (error) {
      console.error(`Failed to load code page table from ${filePath}:`, error);
      throw new Error(`Failed to load code page table: ${filePath}`);
    }
  }

  private getCodePageFilePath(encoding: EncodingType, mode: ConversionMode): string {
    // Return the appropriate code page file path based on encoding and conversion mode
    const basePath = '/codepages/';
    
    switch (encoding) {
      case EncodingType.US:
        return mode === ConversionMode.EBCDIC_TO_ASCII 
          ? `${basePath}EBCASCUS.txt` 
          : `${basePath}ASCEBCUS.txt`;
      
      case EncodingType.JP:
        return mode === ConversionMode.EBCDIC_TO_ASCII 
          ? `${basePath}EBCASCJP.txt` 
          : `${basePath}ASCEBCJP.txt`;
      
      case EncodingType.JAK:
        return mode === ConversionMode.EBCDIC_TO_ASCII 
          ? `${basePath}JEFASCK.txt` 
          : `${basePath}ASCJEFK.txt`;
      
      case EncodingType.KEIS:
        return mode === ConversionMode.EBCDIC_TO_ASCII 
          ? `${basePath}KEISASCK.txt` 
          : `${basePath}ASCJEISK.txt`;
      
      case EncodingType.KR:
        // Use US tables as fallback for KR (no specific KR tables provided)
        return mode === ConversionMode.EBCDIC_TO_ASCII 
          ? `${basePath}EBCASCUS.txt` 
          : `${basePath}ASCEBCUS.txt`;
      
      default:
        return mode === ConversionMode.EBCDIC_TO_ASCII 
          ? `${basePath}EBCASCUS.txt` 
          : `${basePath}ASCEBCUS.txt`;
    }
  }

  public async convert(
    input: string | Uint8Array, 
    options: ConversionOptions,
    debugCallback?: (message: string) => void
  ): Promise<{ output: Uint8Array | string; errors: string[] }> {
    await this.initializeConverters();

    const mapKey = `${options.encoding}_${options.mode === ConversionMode.EBCDIC_TO_ASCII ? 'EBC_TO_ASC' : 'ASC_TO_EBC'}`;
    debugCallback?.(`변환 맵 키: ${mapKey}`);
    
    const conversionMap = this.conversionMaps.get(mapKey);

    if (!conversionMap) {
      debugCallback?.(`❌ 변환 맵을 찾을 수 없음: ${mapKey}`);
      throw new Error(`Conversion map not found for ${mapKey}`);
    }

    debugCallback?.(`✅ 변환 맵 로드 완료`);
    debugCallback?.(`변환 타입: ${conversionMap.convertType}`);

    const errors: string[] = [];
    
    if (options.mode === ConversionMode.EBCDIC_TO_ASCII) {
      return this.convertEBCDICToASCII(input as string, conversionMap, options, errors, debugCallback);
    } else {
      return this.convertASCIIToEBCDIC(input as string, conversionMap, options, errors, debugCallback);
    }
  }

  private convertEBCDICToASCII(
    hexInput: string, 
    map: ConversionMap, 
    options: ConversionOptions,
    errors: string[],
    debugCallback?: (message: string) => void
  ): { output: string; errors: string[] } {
    const cleanInput = hexInput.replace(/\s/g, '');
    debugCallback?.(`정제된 입력: ${cleanInput}`);
    const bytes: number[] = [];
    
    // Parse hex input
    debugCallback?.(`헥스 파싱 시작...`);
    for (let i = 0; i < cleanInput.length; i += 2) {
      const hex = cleanInput.substr(i, 2);
      const byte = parseInt(hex, 16);
      if (isNaN(byte)) {
        debugCallback?.(`❌ 잘못된 헥스 바이트: ${hex}`);
        if (options.errorHandling === ErrorHandling.STRICT) {
          throw new Error(`Invalid hex byte: ${hex}`);
        }
        errors.push(`Invalid hex byte: ${hex}`);
        continue;
      }
      bytes.push(byte);
    }

    debugCallback?.(`파싱된 바이트 수: ${bytes.length}`);
    debugCallback?.(`바이트 배열: [${bytes.map(b => '0x' + b.toString(16).toUpperCase()).join(', ')}]`);
    
    // Check for any input validation issues
    if (cleanInput.length % 2 !== 0) {
      debugCallback?.(`⚠️ 경고: 입력 길이가 홀수입니다 (${cleanInput.length}). 마지막 바이트가 불완전할 수 있습니다.`);
    }

    let result = '';
    let i = 0;

    debugCallback?.(`바이트별 변환 시작...`);
    
    // SOSI processing variables
    let sosiCodes: { SO: number; SI: number };
    
    if (options.sosiType === 'custom') {
      const customSO = parseInt(options.customSO || '0E', 16);
      const customSI = parseInt(options.customSI || '0F', 16);
      sosiCodes = { SO: customSO, SI: customSI };
      debugCallback?.(`커스텀 SOSI 코드 파싱: SO=${options.customSO}(${customSO}), SI=${options.customSI}(${customSI})`);
    } else if (options.sosiType === '1E1F') {
      sosiCodes = { SO: 0x1E, SI: 0x1F };
    } else {
      sosiCodes = { SO: 0x0E, SI: 0x0F };
    }
    
    let isInDoubleByte = false;
    
    debugCallback?.(`SOSI 모드: ${options.useSOSI ? '활성화' : '비활성화'}`);
    if (options.useSOSI) {
      debugCallback?.(`SOSI 코드: SO=0x${sosiCodes.SO.toString(16).toUpperCase()}, SI=0x${sosiCodes.SI.toString(16).toUpperCase()}`);
    } else {
      // Check if input contains potential SOSI codes and warn user
      const hasPotentialSOSI = bytes.some(byte => 
        byte === 0x0E || byte === 0x0F || byte === 0x1E || byte === 0x1F
      );
      if (hasPotentialSOSI) {
        debugCallback?.(`⚠️ 경고: 입력에 잠재적 SOSI 코드가 포함됨 (0x0E, 0x0F, 0x1E, 0x1F). SOSI 모드를 활성화하면 올바른 변환이 가능할 수 있습니다.`);
      }
    }
    
    while (i < bytes.length) {
      const byte = bytes[i];
      
      if (options.useSOSI) {
        // Check for SOSI control characters first, before processing data
        if (byte === sosiCodes.SO) {
          debugCallback?.(`SOSI: Shift-Out (0x${byte.toString(16).toUpperCase()}) - 더블바이트 모드 시작`);
          isInDoubleByte = true;
          
          // Handle SOSI code based on user preference
          const sosiHandling = options.sosiHandling || 'remove';
          if (sosiHandling === 'keep') {
            // Keep the SOSI code as-is
            result += String.fromCharCode(byte);
            debugCallback?.(`  → SOSI 코드 유지: 0x${byte.toString(16).toUpperCase()}`);
          } else if (sosiHandling === 'space') {
            // Convert to space
            result += ' ';
            debugCallback?.(`  → SOSI 코드를 공백으로 변환: 0x20`);
          } else {
            // Remove (default)
            debugCallback?.(`  → SOSI 코드 제거`);
          }
          
          i++;
          continue;
        } else if (byte === sosiCodes.SI) {
          debugCallback?.(`SOSI: Shift-In (0x${byte.toString(16).toUpperCase()}) - 싱글바이트 모드 복귀`);
          isInDoubleByte = false;
          
          // Handle SOSI code based on user preference
          const sosiHandling = options.sosiHandling || 'remove';
          if (sosiHandling === 'keep') {
            // Keep the SOSI code as-is
            result += String.fromCharCode(byte);
            debugCallback?.(`  → SOSI 코드 유지: 0x${byte.toString(16).toUpperCase()}`);
          } else if (sosiHandling === 'space') {
            // Convert to space
            result += ' ';
            debugCallback?.(`  → SOSI 코드를 공백으로 변환: 0x20`);
          } else {
            // Remove (default)
            debugCallback?.(`  → SOSI 코드 제거`);
          }
          
          i++;
          continue;
        }
        
        // Process data based on current SOSI state
        if (isInDoubleByte) {
          // In double-byte mode: ALL data must be processed in 2-byte pairs
          if (i + 1 < bytes.length) {
            const nextByte = bytes[i + 1];
            
            // Check if next byte is SI - if so, treat current byte as single and let SI be processed normally
            if (nextByte === sosiCodes.SI) {
              debugCallback?.(`더블바이트 모드에서 홀수 바이트 0x${byte.toString(16).toUpperCase()} 발견 (다음이 SI)`);
              // Process current byte as single-byte since we can't form a proper double-byte pair
              const converted = map.singleByte[byte];
              const char = String.fromCharCode(converted);
              debugCallback?.(`홀수 바이트를 싱글바이트로 처리: 0x${byte.toString(16).toUpperCase()} → ASCII: 0x${converted.toString(16).toUpperCase()} ('${char}')`);
              result += char;
              i++;
              continue;
            }
            
            // Process as proper double-byte character
            const doubleByteIndex = (byte << 8) | nextByte;
            debugCallback?.(`SOSI 더블바이트: 0x${byte.toString(16).toUpperCase()}${nextByte.toString(16).toUpperCase()}`);
            debugCallback?.(`  더블바이트 인덱스: ${doubleByteIndex} (0x${doubleByteIndex.toString(16).toUpperCase()})`);
            
            if (doubleByteIndex < map.doubleByte.length) {
              const converted = map.doubleByte[doubleByteIndex];
              debugCallback?.(`  매핑된 값: ${converted} (0x${converted.toString(16).toUpperCase()})`);
              
              // Debug: Check if we're getting the expected value
              if (doubleByteIndex === 0x4040) {
                console.log(`Debug 0x4040: map.doubleByte[0x4040] = ${converted} (0x${converted.toString(16).toUpperCase()})`);
                console.log(`Expected: 0x8140 (${0x8140})`);
                console.log(`Array length: ${map.doubleByte.length}`);
                console.log(`Array type: ${typeof map.doubleByte}`);
                console.log(`Array constructor: ${map.doubleByte.constructor.name}`);
              }
              
              // Process double-byte conversion based on actual code page table
              if (converted === 0) {
                // Unmapped double-byte character
                result += '??';
                debugCallback?.(`  ⚠️ 매핑되지 않은 더블바이트: 0x${doubleByteIndex.toString(16).toUpperCase()} → '??'`);
              } else {
                // Convert the mapped value to two ASCII bytes
                const highByte = (converted >> 8) & 0xFF;
                const lowByte = converted & 0xFF;
                
                if (highByte === 0) {
                  // Single byte result
                  result += String.fromCharCode(lowByte);
                  debugCallback?.(`  → ASCII: 0x${lowByte.toString(16).toUpperCase()} ('${String.fromCharCode(lowByte)}')`);
                } else {
                  // Two byte result
                  result += String.fromCharCode(highByte) + String.fromCharCode(lowByte);
                  debugCallback?.(`  → ASCII: 0x${highByte.toString(16).toUpperCase()}${lowByte.toString(16).toUpperCase()} ('${String.fromCharCode(highByte)}${String.fromCharCode(lowByte)}')`);
                }
              }
            } else {
              debugCallback?.(`  → 변환 실패 (범위 초과: ${doubleByteIndex} >= ${map.doubleByte.length})`);
              // Use fallback for out-of-range double-byte
              result += '?';
            }
            i += 2;
            continue;
          } else {
            // Last byte in double-byte mode with no pair - this is an error condition
            debugCallback?.(`⚠️ 더블바이트 모드에서 홀수 바이트 0x${byte.toString(16).toUpperCase()} (마지막 바이트)`);
            const converted = map.singleByte[byte];
            const char = String.fromCharCode(converted);
            debugCallback?.(`홀수 바이트를 싱글바이트로 처리: 0x${byte.toString(16).toUpperCase()} → ASCII: 0x${converted.toString(16).toUpperCase()} ('${char}')`);
            result += char;
            i++;
            continue;
          }
        }
      }
      
      // Single-byte character processing (default for non-SOSI or outside SOSI blocks)
      const converted = map.singleByte[byte];
      const char = String.fromCharCode(converted);
      debugCallback?.(`싱글바이트: 0x${byte.toString(16).toUpperCase()} → ASCII: 0x${converted.toString(16).toUpperCase()} ('${char}')`);
      result += char;
      i++;
    }

    debugCallback?.(`최종 결과: "${result}"`);
    return { output: result, errors };
  }

  private convertASCIIToEBCDIC(
    input: string, 
    map: ConversionMap, 
    options: ConversionOptions,
    errors: string[],
    debugCallback?: (message: string) => void
  ): { output: string; errors: string[] } {
    let result = '';
    debugCallback?.(`ASCII to EBCDIC 변환 시작...`);
    debugCallback?.(`입력 문자열: "${input}"`);

    for (let i = 0; i < input.length; i++) {
      const charCode = input.charCodeAt(i);
      const char = input.charAt(i);
      
      if (charCode > 255) {
        // Handle Unicode characters (would need double-byte mapping)
        debugCallback?.(`❌ 유니코드 문자 발견: '${char}' (0x${charCode.toString(16).toUpperCase()})`);
        this.handleConversionError(charCode, options, errors, result);
        continue;
      }

      const converted = map.singleByte[charCode];
      const hexValue = converted.toString(16).toUpperCase().padStart(2, '0');
      debugCallback?.(`ASCII: '${char}' (0x${charCode.toString(16).toUpperCase()}) → EBCDIC: 0x${hexValue}`);
      result += hexValue;
    }

    debugCallback?.(`최종 EBCDIC 결과: ${result}`);
    return { output: result, errors };
  }

  private handleConversionError(
    byte: number, 
    options: ConversionOptions, 
    errors: string[], 
    result: string
  ): void {
    const errorMsg = `Unable to convert byte: 0x${byte.toString(16).toUpperCase()}`;
    
    switch (options.errorHandling) {
      case ErrorHandling.STRICT:
        throw new Error(errorMsg);
      case ErrorHandling.REPLACE:
        result += '?';
        errors.push(errorMsg);
        break;
      case ErrorHandling.IGNORE:
        errors.push(errorMsg);
        break;
    }
  }

  public getSupportedEncodings(): EncodingType[] {
    return Object.values(EncodingType);
  }

  public isInitialized(): boolean {
    return this.initialized;
  }
}

// Export singleton instance
export const encodingConverter = new EncodingConverter();
export default EncodingConverter;