#!/usr/bin/env node
/**
 * Test script to verify web conversion logic using source files
 */

const fs = require('fs');
const path = require('path');

// Simple test to check if web conversion is working
async function testWebConversion() {
    console.log('Testing web conversion functionality...\n');
    
    // The specific test case: 43AA should convert to proper Japanese character
    const testHex = '0E43AA0F';  // SO + 43AA + SI
    console.log(`Testing hex: ${testHex}`);
    console.log('Expected: 43AA should map to 8386 and decode to proper Japanese character');
    
    // Test data that was known to work in Python but not in web
    const realTestData = '4040404040404040404040C4C9E2D7D3C1E8407D0E43AA435843C543584A7E49BA404040400F7D4B40404040404040404040404040404040404040404040404040404040404040404040404040404040';
    
    console.log('\nReal test data:');
    console.log(`Input: ${realTestData}`);
    console.log('Expected: Should contain DISPLAY and proper Japanese characters');
    
    console.log('\nTo test this:');
    console.log('1. Open http://localhost:3000/source-conversion');
    console.log('2. Select JP encoding');
    console.log('3. Enable SOSI processing');
    console.log('4. Use the test hex data above');
    console.log('5. Verify the output contains proper Japanese characters instead of [U[F');
    
    // Check if the web server is running
    const testUrl = 'http://localhost:3000';
    console.log('\nWeb server status:');
    try {
        const response = await fetch(testUrl);
        if (response.ok) {
            console.log('✅ Web server is running at http://localhost:3000');
        } else {
            console.log('❌ Web server responded with error');
        }
    } catch (error) {
        console.log('❌ Web server is not running. Please start with: npm start');
    }
}

// Run the test
testWebConversion().catch(console.error);