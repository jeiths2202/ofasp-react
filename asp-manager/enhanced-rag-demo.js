// 개선된 RAG 시스템 데모용 테스트 질문들
const demoQueries = [
  {
    query: 'ASP 시스템 로그인 문제 해결하는 방법',
    language: 'ko',
    expectedFeatures: ['TF-IDF 검색', '키워드 매칭', 'N-gram 유사도', '관련도 점수']
  },
  {
    query: 'データベース接続できない',
    language: 'ja', 
    expectedFeatures: ['언어 자동 감지', '일본어 불용어 처리', '다국어 점수 계산']
  },
  {
    query: '커스텀 역할 만들기',
    language: 'ko',
    expectedFeatures: ['서브디렉토리 문서 검색', 'advanced 폴더 참조']
  },
  {
    query: 'パスワードリセット問題',
    language: 'ja',
    expectedFeatures: ['troubleshooting 폴더 참조', '상세 문제 해결 가이드']
  },
  {
    query: 'API 고급 사용법 웹훅',
    language: 'ko',
    expectedFeatures: ['복합 키워드 검색', '정확도 개선', '컨텍스트 분석']
  }
];

console.log('🚀 개선된 RAG 시스템 테스트 가이드');
console.log('===============================\\n');

console.log('💡 새로운 기능들:');
console.log('1. TF-IDF 기반 의미 검색');
console.log('2. N-gram 유사도 계산');
console.log('3. 개선된 언어 감지 및 처리');
console.log('4. 다중 알고리즘 결합 점수');
console.log('5. 상세한 참고 문서 정보');
console.log('6. 관련도 백분율 표시\\n');

console.log('🧪 테스트 질문들:');
demoQueries.forEach((test, index) => {
  console.log(`${index + 1}. "${test.query}" (${test.language})`);
  console.log(`   예상 개선사항: ${test.expectedFeatures.join(', ')}`);
  console.log('');
});

console.log('📊 이전 vs 현재 비교:');
console.log('');
console.log('【이전 시스템】');
console.log('• 단순 키워드 매칭');
console.log('• 기본적인 언어 감지');
console.log('• 제한된 검색 정확도');
console.log('• 단순한 응답 형식');
console.log('');
console.log('【개선된 시스템】'); 
console.log('• TF-IDF + N-gram + 키워드 조합');
console.log('• 고급 언어 감지 및 불용어 처리'); 
console.log('• 가중치 기반 정확도 계산');
console.log('• 관련도 점수 및 상세 출처 정보');
console.log('• 다중 알고리즘 Fallback 시스템');
console.log('');
console.log('🌐 http://localhost:3007 에서 채팅 기능을 테스트해보세요!');