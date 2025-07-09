// 서브디렉토리 문서 검색 테스트
const testQueries = [
  {
    query: '커스텀 역할은 어떻게 만드나요?',
    language: 'ko',
    description: '고급 설정 - 커스텀 역할 생성 테스트'
  },
  {
    query: 'データベースの性能最適化方法を教えてください',
    language: 'ja', 
    description: '고급 설정 - 데이터베이스 튜닝 테스트'
  },
  {
    query: 'パスワードリセットできない問題',
    language: 'ja',
    description: '트러블슈팅 - 패스워드 리셋 문제 테스트'
  },
  {
    query: '화면이 느리게 표시되는 문제 해결',
    language: 'ko',
    description: '트러블슈팅 - 성능 문제 테스트'
  },
  {
    query: 'API 고급 활용 방법',
    language: 'ko',
    description: '고급 설정 - API 활용 테스트'
  }
];

console.log('=== 서브디렉토리 문서 검색 테스트 ===\\n');

testQueries.forEach((test, index) => {
  console.log(`${index + 1}. ${test.description}`);
  console.log(`   질문 (${test.language}): ${test.query}`);
  console.log(`   예상 결과: ${test.language === 'ko' ? '한국어' : '일본어'} 응답, 서브디렉토리 문서 참조`);
  console.log('');
});

console.log('실제 테스트는 http://localhost:3007의 채팅창에서 위 질문들을 입력해보세요.');
console.log('각 질문에 대해 해당 서브디렉토리의 문서가 참조되어 답변이 생성되는지 확인하세요.');