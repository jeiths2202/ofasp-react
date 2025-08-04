// Test EBCDIC conversion functionality
import { encodingConverter, EncodingType, ConversionMode, ErrorHandling } from './src/utils/encodingConverter.js';

async function testEBCDICConversion() {
  console.log('Testing EBCDIC conversion functionality...');
  
  try {
    // Wait for converter to initialize
    console.log('Waiting for converter initialization...');
    await new Promise(resolve => setTimeout(resolve, 2000));
    
    // Test JP encoding conversion
    const testInput = '404081404140'; // EBCDIC hex for some Japanese characters
    const options = {
      mode: ConversionMode.EBCDIC_TO_ASCII,
      encoding: EncodingType.JP,
      useSOSI: true,
      errorHandling: ErrorHandling.REPLACE
    };
    
    console.log(`Input: ${testInput}`);
    console.log(`Encoding: ${options.encoding}`);
    console.log(`Mode: ${options.mode}`);
    
    const result = await encodingConverter.convert(testInput, options, (msg) => {
      console.log(`DEBUG: ${msg}`);
    });
    
    console.log('Conversion result:', result);
    
    // Test all encoding types
    const encodings = [EncodingType.US, EncodingType.JP, EncodingType.JAK, EncodingType.KEIS];
    
    for (const encoding of encodings) {
      console.log(`\nTesting ${encoding} encoding...`);
      try {
        const testOptions = {
          mode: ConversionMode.EBCDIC_TO_ASCII,
          encoding: encoding,
          useSOSI: false,
          errorHandling: ErrorHandling.REPLACE
        };
        
        const simpleTest = await encodingConverter.convert('4040', testOptions);
        console.log(`${encoding}: 0x4040 -> "${simpleTest.output}"`);
      } catch (error) {
        console.error(`${encoding} test failed:`, error.message);
      }
    }
    
  } catch (error) {
    console.error('Test failed:', error);
  }
}

// Run the test
testEBCDICConversion();