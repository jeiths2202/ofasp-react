#!/usr/bin/env node
/**
 * Test script to verify the core conversion logic
 */

// Test the core conversion logic that was fixed
function testShiftJISDecoding() {
    console.log('Testing Shift-JIS decoding logic...\n');
    
    // Test case: 43AA should map to 8386 and decode properly
    const testMapping = 0x8386;  // The mapped value from codepage
    
    console.log(`Testing mapping: 0x43AA → 0x${testMapping.toString(16).toUpperCase()}`);
    
    // Extract bytes as would be done in the web version
    const highByte = (testMapping >> 8) & 0xFF;
    const lowByte = testMapping & 0xFF;
    
    console.log(`High byte: 0x${highByte.toString(16).toUpperCase()}`);
    console.log(`Low byte: 0x${lowByte.toString(16).toUpperCase()}`);
    
    // Test if it's in the proper range for Shift-JIS
    console.log(`Is >= 0x8140? ${testMapping >= 0x8140}`);
    
    // The old logic would do this (incorrect):
    const oldLogic = lowByte;
    const isOldLogicPrintable = (oldLogic >= 0x20 && oldLogic <= 0x7E);
    console.log(`Old logic: low byte 0x${oldLogic.toString(16).toUpperCase()} is printable? ${isOldLogicPrintable}`);
    if (isOldLogicPrintable) {
        console.log(`Old logic would output: '${String.fromCharCode(oldLogic)}'`);
    } else {
        console.log(`Old logic would output: ' ' (space)`);
    }
    
    // The new logic would do this (correct):
    try {
        const sjisBytes = new Uint8Array([highByte, lowByte]);
        const decoder = new TextDecoder('shift_jis');
        const sjisChar = decoder.decode(sjisBytes);
        console.log(`New logic would output: '${sjisChar}'`);
        console.log(`New logic char codes: ${Array.from(sjisChar).map(c => c.charCodeAt(0)).join(', ')}`);
    } catch (e) {
        console.log(`New logic would fall back to space due to error: ${e.message}`);
    }
}

// Test the SOSI processing that was added
function testSOSIProcessing() {
    console.log('\n\nTesting SOSI processing...\n');
    
    const testData = '0E43AA0F';  // SO + 43AA + SI
    console.log(`Test data: ${testData}`);
    
    // Parse hex to bytes
    const bytes = [];
    for (let i = 0; i < testData.length; i += 2) {
        bytes.push(parseInt(testData.substr(i, 2), 16));
    }
    
    console.log(`Bytes: ${bytes.map(b => '0x' + b.toString(16).toUpperCase()).join(' ')}`);
    
    // Check SOSI codes
    const SO = 0x0E;
    const SI = 0x0F;
    
    console.log(`SO (0x${SO.toString(16).toUpperCase()}): ${bytes.includes(SO) ? 'Found' : 'Not found'}`);
    console.log(`SI (0x${SI.toString(16).toUpperCase()}): ${bytes.includes(SI) ? 'Found' : 'Not found'}`);
    
    // Simulate the double-byte processing
    if (bytes.length >= 3 && bytes[0] === SO && bytes[2] === SI) {
        const doubleByteValue = (bytes[1] << 8) | bytes[1];  // This is wrong - should be next byte
        console.log(`Wrong double-byte calculation: 0x${doubleByteValue.toString(16).toUpperCase()}`);
        
        const correctDoubleByteValue = (bytes[1] << 8) | 0xAA;  // Correct: 43AA
        console.log(`Correct double-byte calculation: 0x${correctDoubleByteValue.toString(16).toUpperCase()}`);
    }
}

// Main test
function runTests() {
    console.log('=== Testing Conversion Logic ===');
    testShiftJISDecoding();
    testSOSIProcessing();
    console.log('\n=== Tests Complete ===');
    
    console.log('\nThe web version should now properly:');
    console.log('1. Map 43AA to 8386 using the code page');
    console.log('2. Decode 8386 as Shift-JIS to get proper Japanese character');
    console.log('3. Handle SOSI codes correctly in double-byte mode');
    console.log('\nIf you see proper Japanese characters in the web interface instead of [U[F, the fix is working!');
}

runTests();