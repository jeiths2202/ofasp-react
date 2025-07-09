// Simple test script to demonstrate RAG functionality
const { ragService } = require('./src/services/ragService.ts');

// Test function
async function testRAG() {
  console.log('Testing RAG Service...\n');

  const testQueries = [
    { query: 'ASP 시스템에 로그인하는 방법은?', language: 'ko' },
    { query: 'データベース接続設定について教えてください', language: 'ja' },
    { query: '비밀번호 정책은 어떻게 되나요?', language: 'ko' },
    { query: 'エラーコードERR_001について', language: 'ja' },
    { query: '성능 문제 해결 방법', language: 'ko' }
  ];

  for (const test of testQueries) {
    console.log(`Query (${test.language}): ${test.query}`);
    try {
      const response = await ragService.generateResponse(test.query, test.language);
      console.log(`Response: ${response.answer.substring(0, 200)}...`);
      console.log(`Sources found: ${response.sources.length}`);
    } catch (error) {
      console.log(`Error: ${error.message}`);
    }
    console.log('-'.repeat(80));
  }
}

// Run test if this script is executed directly
if (typeof window === 'undefined') {
  testRAG().catch(console.error);
}

module.exports = { testRAG };