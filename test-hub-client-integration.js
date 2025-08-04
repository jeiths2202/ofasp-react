#!/usr/bin/env node

/**
 * WebSocket Hub ↔ React Client 통합 테스트
 * 
 * 이 스크립트는 새로운 Hub 아키텍처가 React 클라이언트와 
 * 올바르게 연동되는지 검증합니다.
 */

const io = require('socket.io-client');

console.log('🧪 WebSocket Hub ↔ React Client 통합 테스트 시작');
console.log('=' .repeat(60));

// 테스트 설정
const HUB_URL = 'http://localhost:8000';
const TEST_TIMEOUT = 10000; // 10초

// 테스트 결과 추적
const testResults = {
  hubConnection: false,
  hubRegistration: false,
  smedDataDirect: false,
  hubKeyEvent: false,
  hubCommand: false
};

// 클라이언트 시뮬레이션
let client = null;
let testTimer = null;

async function runIntegrationTest() {
  try {
    console.log(`📡 Hub 서버 연결 시도: ${HUB_URL}`);
    
    // WebSocket 클라이언트 생성 - React 클라이언트와 동일한 설정
    client = io(HUB_URL, {
      transports: ['websocket'],
      reconnection: true,
      reconnectionAttempts: 5,
      reconnectionDelay: 1000,
      timeout: 5000,
      forceNew: true,
    });

    // 테스트 타임아웃 설정
    testTimer = setTimeout(() => {
      console.log('\n⏰ 테스트 타임아웃 (10초)');
      printTestResults();
      process.exit(1);
    }, TEST_TIMEOUT);

    // 1. Hub 연결 테스트
    client.on('connect', () => {
      console.log('✅ Hub 연결 성공');
      testResults.hubConnection = true;
      
      // 2. Hub 등록 테스트 (React 클라이언트와 동일)
      console.log('📝 Hub 등록 시도...');
      client.emit('hub_register', {
        terminal_id: 'test_webui',
        user: 'test_user',
        wsname: 'TEST_WS',
        client_type: 'react_web_terminal_test',
        hub_version: 'v2.0'
      });
    });

    client.on('disconnect', (reason) => {
      console.log('❌ Hub 연결 해제:', reason);
    });

    client.on('connect_error', (error) => {
      console.log('❌ Hub 연결 에러:', error.message);
      printTestResults();
      process.exit(1);
    });

    // 3. Hub 등록 응답 테스트
    client.on('hub_registered', (data) => {
      console.log('✅ Hub 등록 완료:', data);
      testResults.hubRegistration = true;
      
      // 4. SMED 데이터 직접 전송 테스트
      testSmedDataDirect();
    });

    // 5. smed_data_direct 이벤트 수신 테스트 (핵심!)
    client.on('smed_data_direct', (data) => {
      console.log('✅ smed_data_direct 이벤트 수신:', {
        action: data.action,
        hub_version: data.hub_version,
        data_flow_type: data.data_flow_type,
        map_file: data.map_file,
        fields_count: data.fields ? Object.keys(data.fields).length : 0
      });
      testResults.smedDataDirect = true;
      
      // 6. Hub 키 이벤트 테스트
      testHubKeyEvent();
    });

    // 7. Hub 상태 이벤트
    client.on('hub_status', (data) => {
      console.log('📊 Hub 상태 업데이트:', data);
    });

    // 8. 에러 처리
    client.on('error', (error) => {
      console.log('❌ Hub 에러:', error);
    });

  } catch (error) {
    console.error('❌ 테스트 실행 에러:', error);
    process.exit(1);
  }
}

// MSGSAMPLEBROWSERMENU 명령 테스트
function testSmedDataDirect() {
  console.log('🔄 MSGSAMPLEBROWSERMENU 명령 테스트...');
  
  // React 클라이언트와 동일한 방식으로 명령 전송
  client.emit('hub_command', {
    command: 'CALL MSGSAMPLEBROWSERMENU',
    terminal_id: 'test_webui',
    user: 'test_user',
    wsname: 'TEST_WS',
    timestamp: new Date().toISOString()
  });
  
  testResults.hubCommand = true;
}

// Hub 키 이벤트 테스트  
function testHubKeyEvent() {
  console.log('⌨️  Hub 키 이벤트 테스트...');
  
  // React 클라이언트와 동일한 방식으로 키 이벤트 전송
  client.emit('hub_key_event', {
    terminal_id: 'test_webui',
    user: 'test_user',
    wsname: 'TEST_WS',
    key: 'F3',
    field_values: { 'test_field': 'test_value' },
    timestamp: new Date().toISOString()
  });
  
  testResults.hubKeyEvent = true;
  
  // 테스트 완료 대기
  setTimeout(() => {
    console.log('\n🎉 모든 테스트 완료!');
    printTestResults();
    process.exit(0);
  }, 2000);
}

// 테스트 결과 출력
function printTestResults() {
  console.log('\n📋 테스트 결과 요약:');
  console.log('=' .repeat(40));
  
  Object.entries(testResults).forEach(([test, passed]) => {
    const icon = passed ? '✅' : '❌';
    const status = passed ? 'PASS' : 'FAIL';
    console.log(`${icon} ${test}: ${status}`);
  });
  
  const passedTests = Object.values(testResults).filter(Boolean).length;
  const totalTests = Object.keys(testResults).length;
  
  console.log('\n📊 전체 결과:');
  console.log(`통과: ${passedTests}/${totalTests}`);
  
  if (passedTests === totalTests) {
    console.log('🎉 모든 테스트 통과! Hub ↔ Client 연동 성공!');
  } else {
    console.log('⚠️  일부 테스트 실패. 서버 상태를 확인하세요.');
  }
  
  // 정리
  if (testTimer) clearTimeout(testTimer);
  if (client) client.disconnect();
}

// 신호 처리
process.on('SIGINT', () => {
  console.log('\n⏹️  테스트 중단됨');
  printTestResults();
  process.exit(0);
});

process.on('SIGTERM', () => {
  console.log('\n⏹️  테스트 종료됨');
  printTestResults();
  process.exit(0);
});

// 사용법 출력
if (process.argv.includes('--help') || process.argv.includes('-h')) {
  console.log(`
사용법: node test-hub-client-integration.js

이 스크립트는 다음을 테스트합니다:
  1. Hub 서버 연결
  2. Hub 클라이언트 등록  
  3. smed_data_direct 이벤트 수신
  4. Hub 키 이벤트 전송
  5. Hub 명령 전송

서버가 실행 중인지 확인하세요:
  python test_websocket_hub.py
`);
  process.exit(0);
}

// 테스트 시작
console.log('🚀 통합 테스트 시작...\n');
runIntegrationTest();