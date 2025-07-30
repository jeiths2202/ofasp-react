import React, { useState, useRef, useEffect } from 'react';
import {
  PaperAirplaneIcon,
  UserIcon,
  SparklesIcon,
  ArrowPathIcon,
  TrashIcon,
  BugAntIcon,
} from '@heroicons/react/24/outline';
import classNames from 'classnames';
import { ragService } from '../services/ragService';
import { enhancedRagService } from '../services/enhancedRagService';
import { advancedRAGSystem } from '../services/advancedRagSystem';

interface Message {
  id: string;
  type: 'user' | 'assistant';
  content: string;
  timestamp: Date;
  language?: 'ko' | 'ja';
}

interface ChatInterfaceProps {
  isDarkMode: boolean;
}

const ChatInterface: React.FC<ChatInterfaceProps> = ({ isDarkMode }) => {
  const [messages, setMessages] = useState<Message[]>([
    {
      id: '1',
      type: 'assistant',
      content: 'ASPマニュアルについて何でもお聞きください。日本語でも韓国語でも対応いたします。',
      timestamp: new Date(),
      language: 'ja',
    },
  ]);
  const [inputMessage, setInputMessage] = useState('');
  const [isLoading, setIsLoading] = useState(false);
  const [isTyping, setIsTyping] = useState(false);
  const messagesEndRef = useRef<HTMLDivElement>(null);
  const inputRef = useRef<HTMLTextAreaElement>(null);

  const scrollToBottom = () => {
    messagesEndRef.current?.scrollIntoView({ behavior: 'smooth' });
  };

  useEffect(() => {
    scrollToBottom();
  }, [messages]);

  const detectLanguage = (text: string): 'ko' | 'ja' => {
    // Simple language detection based on character sets
    const koreanRegex = /[ㄱ-ㅎ|ㅏ-ㅣ|가-힣]/;
    const japaneseRegex = /[ひらがな|カタカナ|漢字]/;
    
    if (koreanRegex.test(text)) return 'ko';
    if (japaneseRegex.test(text)) return 'ja';
    return 'ko'; // Default to Korean
  };

  // Phase 3 고급 RAG 시스템 완전 초기화
  useEffect(() => {
    let isInitialized = false;
    
    const initializeAdvancedRAG = async () => {
      if (isInitialized) {
        console.log('🔄 이미 초기화 중이거나 완료된 상태입니다');
        return;
      }
      
      isInitialized = true;
      
      try {
        console.log('🔄 벡터 DB 완전 초기화 및 실제 PDF 문서 로드 시작...');
        
        // 벡터 DB 완전 초기화 및 재구축
        await advancedRAGSystem.resetAndRebuild();
        
        // 시스템 상태 확인
        const status = advancedRAGSystem.getSystemStatus();
        console.log('📊 초기화 후 시스템 상태:', status);
        
        // 시스템 진단 실행
        const diagnosis = await advancedRAGSystem.diagnose();
        console.log('🔍 시스템 진단:', diagnosis);
        
        if (status.totalDocuments > 0) {
          console.log('✅ 벡터 DB가 완전히 초기화되고 실제 PDF 문서들이 로드되었습니다！');
          console.log(`📁 총 ${status.totalDocuments}개 문서가 벡터 DB에 등록되었습니다`);
        } else {
          console.warn('⚠️ 문서가 로드되지 않았습니다. API 서버 연결을 확인하세요');
        }
      } catch (error) {
        console.error('❌ 벡터 DB 초기화 오류:', error);
        console.error('💡 API 서버(포트 3008)가 실행 중인지 확인하세요');
        console.error('🔧 API 서버를 시작한 후 페이지를 새로고침하세요');
        isInitialized = false;
      }
    };
    
    // API 서버 연결 확인 후 초기화
    const checkApiAndInitialize = async () => {
      try {
        // 프록시를 통한 상대 경로로 접근
        const response = await fetch('/api/health');
        if (response.ok) {
          console.log('✅ API 서버 연결 확인됨 (프록시 통해)');
          await initializeAdvancedRAG();
        } else {
          throw new Error('API 서버 응답 오류');
        }
      } catch (error) {
        console.error('❌ API 서버 연결 실패:', error);
        console.error('🔧 API 서버(포트 3008)를 시작해주세요');
        console.error('💡 React 앱을 재시작하여 프록시 설정을 적용해주세요');
      }
    };
    
    checkApiAndInitialize();
    
    // Cleanup function
    return () => {
      isInitialized = false;
    };
  }, []);

  const getRAGResponse = async (query: string, language: 'ko' | 'ja'): Promise<string> => {
    // 특별한 디버그 명령어 처리
    if (query === '/debug-db' || query === '/벡터DB확인') {
      const status = advancedRAGSystem.getSystemStatus();
      return `📊 **벡터 DB 상태 보고서**

🔢 **기본 통계**
- 총 문서 수: ${status.totalDocuments}개
- 총 임베딩 수: ${status.totalEmbeddings}개  
- 시스템 상태: ${status.systemHealth}
- 마지막 스캔: ${status.lastScanTime?.toLocaleString() || 'N/A'}

⚡ **성능 지표**
- 평균 쿼리 시간: ${Math.round(status.performance.avgQueryTime)}ms
- 평균 정확도: ${Math.round(status.performance.avgAccuracy * 100)}%
- 총 쿼리 수: ${status.performance.totalQueries}회

🧪 **테스트 권장 질문**
1. "ASP가 뭐야?" - 기본 매뉴얼 확인
2. "COBOL 문법을 알려줘" - PDF 문서 확인  
3. "시스템 명령어는?" - 대용량 PDF 확인
4. "파일 처리 방법은?" - 기술 문서 확인

📝 **추가 명령어**
- \`/debug-search 키워드\` - 특정 키워드 검색 테스트
- 개발자 도구 콘솔에서 상세 로그 확인 가능`;
    }
    
    // 특별한 검색 디버그 명령어
    if (query.startsWith('/debug-search ')) {
      const searchTerm = query.replace('/debug-search ', '');
      console.log(`🔍 디버그 검색 실행: "${searchTerm}"`);
      
      // 벡터 검색 결과만 반환
      const advancedResponse = await advancedRAGSystem.query(searchTerm, language, true);
      
      let debugInfo = `🔍 **검색 디버그 결과: "${searchTerm}"**\n\n`;
      debugInfo += `📊 **검색 통계**\n`;
      debugInfo += `- 처리 시간: ${advancedResponse.systemInfo.processingTime}ms\n`;
      debugInfo += `- 검색된 문서: ${advancedResponse.systemInfo.vectorSearchResults}개\n`;
      debugInfo += `- 신뢰도: ${Math.round(advancedResponse.systemInfo.confidence * 100)}%\n\n`;
      
      debugInfo += `📑 **검색된 소스**\n`;
      advancedResponse.sources.slice(0, 3).forEach((source, index) => {
        debugInfo += `${index + 1}. ${source.chunk.metadata.source} (점수: ${source.score.toFixed(3)})\n`;
        debugInfo += `   내용: ${source.chunk.content.substring(0, 100)}...\n\n`;
      });
      
      return debugInfo;
    }
    
    try {
      console.log(`🧠 Phase 3 고급 RAG 처리: "${query}"`);
      
      // Phase 3: 고급 RAG 시스템 사용
      const advancedResponse = await advancedRAGSystem.query(query, language, true);
      
      // Chain-of-Thought 추론 정보 포함
      let response = advancedResponse.answer;
      
      // 출처 정보 추가 (항상 표시)
      if (advancedResponse.sources && advancedResponse.sources.length > 0) {
        response += `\n\n📚 **참고 문서**`;
        advancedResponse.sources.slice(0, 3).forEach((source, index) => {
          const fileName = source.chunk.metadata.source.split('/').pop() || source.chunk.metadata.source;
          const confidence = Math.round(source.score * 100);
          response += `\n${index + 1}. ${fileName} (신뢰도: ${confidence}%)`;
          
          // 문서 미리보기 추가
          const preview = source.chunk.content.substring(0, 80).replace(/\n/g, ' ');
          response += `\n   └ "${preview}..."`;
        });
      }
      
      // 시스템 정보 추가 (디버그 모드에서)
      if (advancedResponse.systemInfo.confidence > 0.8) {
        response += `\n\n🎯 **시스템 정보**`;
        response += `\n• 처리 시간: ${advancedResponse.systemInfo.processingTime}ms`;
        response += `\n• 검색된 문서: ${advancedResponse.systemInfo.vectorSearchResults}개`;
        response += `\n• 추론 단계: ${advancedResponse.systemInfo.reasoningSteps}단계`;
        response += `\n• 신뢰도: ${Math.round(advancedResponse.systemInfo.confidence * 100)}%`;
        
        // Chain-of-Thought 요약 추가 (높은 신뢰도일 때만)
        if (advancedResponse.reasoning.steps.length > 0) {
          response += `\n\n🧠 **추론 과정 요약**`;
          advancedResponse.reasoning.steps.forEach((step, index) => {
            response += `\n${index + 1}. ${step.description} (신뢰도: ${Math.round(step.confidence * 100)}%)`;
          });
        }
      }
      
      return response;
      
    } catch (error) {
      console.error('❌ Phase 3 RAG 시스템 오류:', error);
      
      // Fallback Level 1: Enhanced RAG 시스템 사용
      try {
        console.log('⚠️ Fallback Level 1: Enhanced RAG 시스템 사용');
        const enhancedResponse = await enhancedRagService.generateResponse(query, language);
        let fallbackResponse = enhancedResponse.answer;
        
        // Enhanced RAG 출처 정보 추가
        if (enhancedResponse.sources && enhancedResponse.sources.length > 0) {
          fallbackResponse += `\n\n📚 **참고 문서**`;
          enhancedResponse.sources.slice(0, 3).forEach((source, index) => {
            const fileName = source.chunk.metadata.source.split('/').pop() || source.chunk.metadata.source;
            const confidence = Math.round(source.score * 100);
            fallbackResponse += `\n${index + 1}. ${fileName} (신뢰도: ${confidence}%)`;
          });
        }
        
        return fallbackResponse + '\n\n⚠️ Enhanced 검색 모드로 응답되었습니다。';
      } catch (enhancedError) {
        console.error('Enhanced RAG service error:', enhancedError);
        
        // Fallback Level 2: 기본 RAG 서비스 사용
        try {
          console.log('⚠️ Fallback Level 2: 기본 RAG 시스템 사용');
          const basicResponse = await ragService.generateResponse(query, language);
          let basicFallbackResponse = basicResponse.answer;
          
          // 기본 RAG 출처 정보 추가
          if (basicResponse.sources && basicResponse.sources.length > 0) {
            basicFallbackResponse += `\n\n📚 **참고 문서**`;
            basicResponse.sources.slice(0, 3).forEach((source, index) => {
              const fileName = source.chunk.metadata.source.split('/').pop() || source.chunk.metadata.source;
              const confidence = Math.round(source.score * 100);
              basicFallbackResponse += `\n${index + 1}. ${fileName} (신뢰도: ${confidence}%)`;
            });
          }
          
          return basicFallbackResponse + '\n\n⚠️ 기본 검색 모드로 응답되었습니다。';
        } catch (basicError) {
          console.error('Basic RAG service error:', basicError);
          
          const fallbackMessage = language === 'ko'
            ? '죄송합니다. 현재 문서 검색 서비스에 일시적인 문제가 있습니다. 잠시 후 다시 시도해주세요.'
            : '申し訳ございません。現在、文書検索サービスに一時的な問題があります。しばらくしてから再度お試しください。';
          
          return fallbackMessage;
        }
      }
    }
  };

  const handleSendMessage = async () => {
    if (!inputMessage.trim() || isLoading) return;

    const userMessage: Message = {
      id: Date.now().toString(),
      type: 'user',
      content: inputMessage.trim(),
      timestamp: new Date(),
      language: detectLanguage(inputMessage.trim()),
    };

    setMessages(prev => [...prev, userMessage]);
    setInputMessage('');
    setIsLoading(true);
    setIsTyping(true);

    try {
      const response = await getRAGResponse(userMessage.content, userMessage.language || 'ko');
      
      const assistantMessage: Message = {
        id: (Date.now() + 1).toString(),
        type: 'assistant',
        content: response,
        timestamp: new Date(),
        language: userMessage.language || 'ko',
      };

      setMessages(prev => [...prev, assistantMessage]);
    } catch (error) {
      console.error('Error getting response:', error);
      const errorMessage: Message = {
        id: (Date.now() + 1).toString(),
        type: 'assistant',
        content: userMessage.language === 'ko' 
          ? '죄송합니다. 응답을 생성하는 중에 오류가 발생했습니다.' 
          : '申し訳ございません。応答の生成中にエラーが発生しました。',
        timestamp: new Date(),
        language: userMessage.language || 'ko',
      };
      setMessages(prev => [...prev, errorMessage]);
    } finally {
      setIsLoading(false);
      setIsTyping(false);
    }
  };

  const handleKeyPress = (e: React.KeyboardEvent) => {
    if (e.key === 'Enter' && !e.shiftKey) {
      e.preventDefault();
      handleSendMessage();
    }
  };

  const clearMessages = () => {
    setMessages([
      {
        id: '1',
        type: 'assistant',
        content: 'ASPマニュアルについて何でもお聞きください。日本語でも韓国語でも対応いたします。',
        timestamp: new Date(),
        language: 'ja',
      },
    ]);
  };

  const debugVectorDB = () => {
    console.log('🔍 벡터 DB 디버그 정보를 콘솔에 출력합니다...');
    const status = advancedRAGSystem.getSystemStatus();
    console.log('시스템 상태:', status);
    
    // 상세 디버그 메시지들
    console.log('\n=== 📊 벡터 DB 확인 방법들 ===');
    console.log('1. 아래 테스트 질문들로 문서 로딩 확인:');
    console.log('   • "ASP가 뭐야?" - 기본 매뉴얼 확인');
    console.log('   • "COBOL 문법을 알려줘" - COBOL 문법서 확인');
    console.log('   • "시스템 명령어는?" - 시스템 명령어집 확인');
    console.log('   • "파일 처리 방법은?" - 파일 설명서 확인');
    console.log('   • "프로그램 개발 절차는?" - 개발 가이드 확인');
    console.log('\n2. 질문 시 콘솔에서 다음 정보 확인:');
    console.log('   • 벡터 검색 결과 (유사도 점수)');
    console.log('   • 문서 내용 미리보기');
    console.log('   • Chain-of-Thought 추론 과정');
    console.log('\n3. 시스템 정보:');
    console.log(`   • 총 문서 수: ${status.totalDocuments}개`);
    console.log(`   • 총 임베딩 수: ${status.totalEmbeddings}개`);
    console.log(`   • 시스템 상태: ${status.systemHealth}`);
    console.log(`   • 평균 쿼리 시간: ${Math.round(status.performance.avgQueryTime)}ms`);
    console.log(`   • 평균 정확도: ${Math.round(status.performance.avgAccuracy * 100)}%`);
  };

  return (
    <div className={classNames(
      'flex flex-col h-full',
      isDarkMode ? 'bg-gray-900' : 'bg-gray-50'
    )}>
      {/* Header */}
      <div className={classNames(
        'flex items-center justify-between p-6 border-b',
        isDarkMode 
          ? 'border-gray-700 bg-gray-800' 
          : 'border-gray-200 bg-white'
      )}>
        <div className="flex items-center space-x-3">
          <div className={classNames(
            'w-10 h-10 rounded-full flex items-center justify-center',
            isDarkMode ? 'bg-blue-600' : 'bg-blue-500'
          )}>
            <SparklesIcon className="w-6 h-6 text-white" />
          </div>
          <div>
            <h2 className={classNames(
              'text-lg font-semibold',
              isDarkMode ? 'text-white' : 'text-gray-900'
            )}>
              ASP マニュアル アシスタント
            </h2>
            <p className={classNames(
              'text-sm',
              isDarkMode ? 'text-gray-400' : 'text-gray-500'
            )}>
              RAGベース文書検索 • 日本語/韓国語対応
            </p>
          </div>
        </div>
        <div className="flex items-center space-x-2">
          <button
            onClick={debugVectorDB}
            className={classNames(
              'p-2 rounded-lg transition-colors',
              isDarkMode 
                ? 'hover:bg-gray-700 text-gray-400 hover:text-white' 
                : 'hover:bg-gray-100 text-gray-500 hover:text-gray-700'
            )}
            title="ベクターDBデバッグ"
          >
            <BugAntIcon className="w-5 h-5" />
          </button>
          <button
            onClick={clearMessages}
            className={classNames(
              'p-2 rounded-lg transition-colors',
              isDarkMode 
                ? 'hover:bg-gray-700 text-gray-400 hover:text-white' 
                : 'hover:bg-gray-100 text-gray-500 hover:text-gray-700'
            )}
            title="会話をクリア"
          >
            <TrashIcon className="w-5 h-5" />
          </button>
        </div>
      </div>

      {/* Messages */}
      <div className="flex-1 overflow-y-auto p-6 space-y-4">
        {messages.map((message) => (
          <div key={message.id} className={classNames(
            'flex items-start space-x-3',
            message.type === 'user' ? 'justify-end' : 'justify-start'
          )}>
            {message.type === 'assistant' && (
              <div className={classNames(
                'w-8 h-8 rounded-full flex items-center justify-center flex-shrink-0',
                isDarkMode ? 'bg-blue-600' : 'bg-blue-500'
              )}>
                <SparklesIcon className="w-4 h-4 text-white" />
              </div>
            )}
            
            <div className={classNames(
              'max-w-xs lg:max-w-md px-4 py-3 rounded-2xl',
              message.type === 'user' 
                ? isDarkMode 
                  ? 'bg-blue-600 text-white' 
                  : 'bg-blue-500 text-white'
                : isDarkMode 
                  ? 'bg-gray-800 text-gray-100 border border-gray-700' 
                  : 'bg-white text-gray-900 border border-gray-200 shadow-sm'
            )}>
              <p className="text-sm leading-relaxed whitespace-pre-wrap">
                {message.content}
              </p>
              <p className={classNames(
                'text-xs mt-2 opacity-70',
                message.type === 'user' ? 'text-blue-100' : isDarkMode ? 'text-gray-400' : 'text-gray-500'
              )}>
                {message.timestamp.toLocaleTimeString()}
              </p>
            </div>

            {message.type === 'user' && (
              <div className={classNames(
                'w-8 h-8 rounded-full flex items-center justify-center flex-shrink-0',
                isDarkMode ? 'bg-gray-700' : 'bg-gray-300'
              )}>
                <UserIcon className={classNames(
                  'w-4 h-4',
                  isDarkMode ? 'text-gray-300' : 'text-gray-600'
                )} />
              </div>
            )}
          </div>
        ))}

        {isTyping && (
          <div className="flex items-start space-x-3">
            <div className={classNames(
              'w-8 h-8 rounded-full flex items-center justify-center flex-shrink-0',
              isDarkMode ? 'bg-blue-600' : 'bg-blue-500'
            )}>
              <SparklesIcon className="w-4 h-4 text-white" />
            </div>
            <div className={classNames(
              'px-4 py-3 rounded-2xl',
              isDarkMode 
                ? 'bg-gray-800 text-gray-100 border border-gray-700' 
                : 'bg-white text-gray-900 border border-gray-200 shadow-sm'
            )}>
              <div className="flex items-center space-x-1">
                <div className={classNames(
                  'w-2 h-2 rounded-full animate-pulse',
                  isDarkMode ? 'bg-gray-400' : 'bg-gray-500'
                )} />
                <div className={classNames(
                  'w-2 h-2 rounded-full animate-pulse',
                  isDarkMode ? 'bg-gray-400' : 'bg-gray-500'
                )} style={{ animationDelay: '0.2s' }} />
                <div className={classNames(
                  'w-2 h-2 rounded-full animate-pulse',
                  isDarkMode ? 'bg-gray-400' : 'bg-gray-500'
                )} style={{ animationDelay: '0.4s' }} />
              </div>
            </div>
          </div>
        )}

        <div ref={messagesEndRef} />
      </div>

      {/* Input */}
      <div className={classNames(
        'p-6 border-t',
        isDarkMode 
          ? 'border-gray-700 bg-gray-800' 
          : 'border-gray-200 bg-white'
      )}>
        <div className="flex items-end space-x-3">
          <div className="flex-1">
            <textarea
              ref={inputRef}
              value={inputMessage}
              onChange={(e) => setInputMessage(e.target.value)}
              onKeyPress={handleKeyPress}
              placeholder="ASPマニュアルについて質問してください... (日本語/한국어)"
              className={classNames(
                'w-full px-4 py-3 rounded-2xl border-2 transition-all duration-200 resize-none',
                'focus:outline-none focus:ring-0',
                isDarkMode 
                  ? 'bg-gray-700 border-gray-600 text-white placeholder-gray-400 focus:border-blue-500' 
                  : 'bg-gray-50 border-gray-200 text-gray-900 placeholder-gray-500 focus:border-blue-400'
              )}
              rows={1}
              style={{ minHeight: '48px', maxHeight: '120px' }}
            />
          </div>
          <button
            onClick={handleSendMessage}
            disabled={!inputMessage.trim() || isLoading}
            className={classNames(
              'p-3 rounded-full transition-all duration-200',
              'focus:outline-none focus:ring-2 focus:ring-offset-2',
              isDarkMode ? 'focus:ring-offset-gray-800' : 'focus:ring-offset-white',
              inputMessage.trim() && !isLoading
                ? isDarkMode 
                  ? 'bg-blue-600 hover:bg-blue-700 text-white focus:ring-blue-500' 
                  : 'bg-blue-500 hover:bg-blue-600 text-white focus:ring-blue-400'
                : isDarkMode 
                  ? 'bg-gray-700 text-gray-500 cursor-not-allowed' 
                  : 'bg-gray-200 text-gray-400 cursor-not-allowed'
            )}
          >
            {isLoading ? (
              <ArrowPathIcon className="w-5 h-5 animate-spin" />
            ) : (
              <PaperAirplaneIcon className="w-5 h-5" />
            )}
          </button>
        </div>
      </div>
    </div>
  );
};

export default ChatInterface;