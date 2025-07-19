#!/usr/bin/env node
/**
 * Test script to verify TypeScript conversion logic with real EBCDIC data
 */

const { encodingConverter, EncodingType } = require('./build/static/js/main.f46231b4.js');

async function testTypescriptConversion() {
    console.log('Testing TypeScript EBCDIC Converter with real data...\n');
    
    // Test data from the actual files that now works correctly
    const testData = '4040404040404040404040C4C9E2D7D3C1E8407D0E43AA435843C543584A7E49BA404040400F7D4B40404040404040404040404040404040404040404040404040404040404040404040404040404040';
    
    try {
        console.log('Input hex:', testData);
        console.log('Testing with JP encoding and SOSI enabled...\n');
        
        const result = await encodingConverter.EBCDIC_TO_ASCII(
            testData,       // input
            null,           // output (not used)
            EncodingType.JP, // encoding type
            true,           // sosi_flag
            false,          // out_sosi_flag
            80,             // rlen (fixed for source conversion)
            null            // layout (not used for source)
        );
        
        console.log('Conversion Result:');
        console.log(`"${result}"`);
        console.log('\nHex representation:');
        const hexResult = Array.from(result).map(char => 
            char.charCodeAt(0).toString(16).padStart(2, '0').toUpperCase()
        ).join(' ');
        console.log(hexResult);
        
        // Check if the result contains expected parts
        if (result.includes('DISPLAY')) {
            console.log('\n✅ SUCCESS: DISPLAY text found');
        } else {
            console.log('\n❌ FAILURE: DISPLAY text not found');
        }
        
        if (result.length === 80) {
            console.log('✅ SUCCESS: Result length is 80 characters (rlen)');
        } else {
            console.log(`❌ FAILURE: Result length is ${result.length}, expected 80`);
        }
        
    } catch (error) {
        console.error('❌ Test failed:', error);
    }
}

// Run if this script is executed directly
if (require.main === module) {
    testTypescriptConversion();
}

module.exports = { testTypescriptConversion };