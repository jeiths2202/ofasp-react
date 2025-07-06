// Phase 3: 실제 TensorFlow.js 기반 고급 RAG 시스템
import { DocumentChunk, RAGResponse } from './ragService';
import { realVectorDatabase, RealVectorDatabase, RealVectorSearchResult } from './realVectorDatabase';
import { VectorSearchResult } from './vectorDatabase';
import { fileSystemScanner, FileSystemScanner, ScanResult } from './fileSystemScanner';
import { chainOfThoughtReasoner, ChainOfThoughtReasoner, ChainOfThoughtResult } from './chainOfThought';

// 시스템 상태
export interface SystemStatus {
  isInitialized: boolean;
  totalDocuments: number;
  totalEmbeddings: number;
  lastScanTime: Date | null;
  lastUpdateTime: Date | null;
  systemHealth: 'excellent' | 'good' | 'warning' | 'error';
  performance: {
    avgQueryTime: number;
    avgAccuracy: number;
    totalQueries: number;
  };
}

// 고급 RAG 응답
export interface AdvancedRAGResponse extends RAGResponse {
  reasoning: ChainOfThoughtResult;
  systemInfo: {
    processingTime: number;
    documentsScanned: number;
    vectorSearchResults: number;
    reasoningSteps: number;
    confidence: number;
  };
  debug?: {
    vectorSimilarities: number[];
    keywordMatches: string[];
    languageDetection: { detected: 'ko' | 'ja'; confidence: number };
  };
}

// 실제 TensorFlow.js 기반 고급 RAG 시스템
export class AdvancedRAGSystem {
  private vectorDB: RealVectorDatabase;
  private fileScanner: FileSystemScanner;
  private reasoner: ChainOfThoughtReasoner;
  
  private isInitialized = false;
  private systemStatus: SystemStatus;
  private queryStats: Array<{ query: string; time: number; accuracy: number; timestamp: Date }> = [];
  
  constructor() {
    this.vectorDB = realVectorDatabase;
    this.fileScanner = fileSystemScanner;
    this.reasoner = chainOfThoughtReasoner;
    
    this.systemStatus = {
      isInitialized: false,
      totalDocuments: 0,
      totalEmbeddings: 0,
      lastScanTime: null,
      lastUpdateTime: null,
      systemHealth: 'warning',
      performance: {
        avgQueryTime: 0,
        avgAccuracy: 0,
        totalQueries: 0
      }
    };
  }
  
  // 실제 벡터 DB 완전 초기화 및 재구축
  async resetAndRebuild(): Promise<void> {
    console.log('🔄 실제 TensorFlow.js 벡터 DB 완전 초기화 및 재구축 시작...');
    
    try {
      // 1. 실제 TensorFlow.js 모델 초기화
      console.log('🤖 TensorFlow.js 모델 초기화 중...');
      await this.vectorDB.initialize();
      
      // 2. 벡터 DB 완전 초기화
      console.log('🗑️ 기존 벡터 DB 데이터 완전 삭제...');
      this.vectorDB.clear();
      
      // 2. 시스템 상태 초기화
      this.systemStatus = {
        isInitialized: false,
        totalDocuments: 0,
        totalEmbeddings: 0,
        lastScanTime: null,
        lastUpdateTime: null,
        systemHealth: 'warning',
        performance: {
          avgQueryTime: 0,
          avgAccuracy: 0,
          totalQueries: 0
        }
      };
      
      // 3. 쿼리 통계 초기화
      this.queryStats = [];
      this.isInitialized = false;
      
      console.log('✅ 벡터 DB 완전 초기화 완료');
      
      // 4. 강제 재스캔으로 초기화
      await this.initialize(true);
      
    } catch (error) {
      console.error('❌ 벡터 DB 초기화 중 오류:', error);
      throw error;
    }
  }

  // 시스템 초기화 (캐싱 지원)
  async initialize(forceRescan: boolean = false): Promise<void> {
    console.log('🚀 고급 RAG 시스템 초기화 시작...');
    const startTime = Date.now();
    
    try {
      // 캐시 상태 확인
      const cacheStatus = this.vectorDB.getCacheStatus();
      console.log('📦 캐시 상태:', cacheStatus);
      
      // 벡터 DB에 데이터가 이미 로드되어 있고 강제 재스캔이 아닌 경우
      if (!forceRescan && cacheStatus.dataCached && cacheStatus.totalDocuments > 0) {
        console.log('✅ 벡터 DB 데이터가 이미 캐시됨 - 초기화 건너뛰기');
        console.log(`📊 캐시된 문서: ${cacheStatus.totalDocuments}개`);
        console.log(`⏰ 마지막 로드: ${cacheStatus.lastLoadTime?.toLocaleString()}`);
        
        // 시스템 상태만 업데이트
        this.systemStatus.isInitialized = true;
        this.systemStatus.totalEmbeddings = cacheStatus.totalDocuments;
        this.systemStatus.systemHealth = 'excellent';
        this.isInitialized = true;
        
        const initTime = Date.now() - startTime;
        console.log(`⚡ 캐시된 시스템 초기화 완료 (${initTime}ms)`);
        return;
      }
      
      // Step 1: 파일 시스템 스캔
      console.log('📂 1단계: 파일 시스템 스캔');
      let scanResult: ScanResult;
      
      if (forceRescan || !this.systemStatus.lastScanTime) {
        scanResult = await this.fileScanner.scanAllManuals();
      } else {
        // 증분 스캔 (마지막 스캔 이후 변경된 파일만)
        scanResult = await this.fileScanner.scanIncremental(this.systemStatus.lastScanTime);
      }
      
      console.log(`📊 스캔 결과: ${scanResult.processedFiles}/${scanResult.totalFiles} 파일 처리`);
      
      // Step 2: 벡터 데이터베이스 구축
      console.log('🔍 2단계: 벡터 임베딩 생성 및 인덱싱');
      
      // TensorFlow.js 모델이 초기화되지 않은 경우 초기화
      if (!this.vectorDB.isReady()) {
        console.log('🤖 TensorFlow.js 벡터 모델 초기화 중...');
        await this.vectorDB.initialize();
      }
      
      // 기존 데이터 정리 (전체 재스캔인 경우)
      if (forceRescan) {
        this.vectorDB.clear();
      }
      
      // 문서들을 실제 벡터 데이터베이스에 추가 (중복 체크 포함)
      console.log(`📝 ${scanResult.documents.length}개 문서를 실제 TensorFlow.js 벡터 DB에 추가 중...`);
      for (const document of scanResult.documents) {
        console.log(`📄 실제 벡터 임베딩 생성: ${document.metadata.source} (${document.metadata.language})`);
        console.log(`📋 내용 미리보기: ${document.content.substring(0, 200)}...`);
        await this.vectorDB.addDocument(document);
      }
      
      // Step 3: 시스템 상태 업데이트
      const dbStats = this.vectorDB.getStats();
      this.systemStatus = {
        isInitialized: true,
        totalDocuments: scanResult.processedFiles,
        totalEmbeddings: dbStats.totalEmbeddings,
        lastScanTime: new Date(),
        lastUpdateTime: new Date(),
        systemHealth: this.assessSystemHealth(scanResult, dbStats),
        performance: this.systemStatus.performance
      };
      
      this.isInitialized = true;
      
      const initTime = Date.now() - startTime;
      console.log(`✅ 고급 RAG 시스템 초기화 완료 (${initTime}ms)`);
      console.log(`📈 시스템 상태: ${this.systemStatus.systemHealth}`);
      console.log(`📊 통계: ${this.systemStatus.totalDocuments}개 문서, ${this.systemStatus.totalEmbeddings}개 임베딩`);
      
      // 벡터 DB 상태 디버그
      console.log('\n🔍 벡터 DB 상태 확인 중...');
      this.vectorDB.debugPrintAllEmbeddings();
      
    } catch (error) {
      console.error('❌ 시스템 초기화 실패:', error);
      this.systemStatus.systemHealth = 'error';
      throw error;
    }
  }
  
  // 고급 RAG 질의 처리
  async query(question: string, language?: 'ko' | 'ja', enableDebug: boolean = false): Promise<AdvancedRAGResponse> {
    if (!this.isInitialized) {
      await this.initialize();
    }
    
    const startTime = Date.now();
    console.log(`🔎 고급 RAG 질의 처리: "${question}"`);
    
    // Step 1: 언어 감지 (지정되지 않은 경우)
    const detectedLanguage = language || this.detectLanguage(question);
    
    try {
      const languageConfidence = this.calculateLanguageConfidence(question, detectedLanguage);
      
      console.log(`🌐 언어 감지: ${detectedLanguage} (신뢰도: ${Math.round(languageConfidence * 100)}%)`);
      
      // Step 2: 실제 TensorFlow.js 벡터 검색
      console.log('🔍 실제 벡터 유사도 검색 수행');
      
      const vectorResults = await this.vectorDB.search(question, detectedLanguage, 10);
      
      console.log(`📄 실제 벡터 검색 결과: ${vectorResults.length}개 문서 발견`);
      vectorResults.forEach((result, index) => {
        console.log(`📑 결과 ${index + 1}: 유사도 ${result.similarity.toFixed(3)} - "${result.embedding.metadata.text.substring(0, 100)}..."`);
      });
      
      // Step 3: Chain-of-Thought 추론
      console.log('🧠 Chain-of-Thought 추론 수행');
      
      // RealVectorSearchResult를 VectorSearchResult로 변환
      const convertedResults: VectorSearchResult[] = vectorResults.map(result => ({
        embedding: {
          id: result.embedding.id,
          vector: Array.from(result.embedding.vector), // Float32Array를 number[]로 변환
          metadata: result.embedding.metadata
        },
        similarity: result.similarity,
        distance: result.distance
      }));
      
      const reasoning = await this.reasoner.reason(question, detectedLanguage, convertedResults);
      
      // Step 4: 고급 응답 생성
      const response = await this.generateAdvancedResponse(
        question, 
        detectedLanguage, 
        convertedResults, 
        reasoning,
        enableDebug
      );
      
      // Step 5: 성능 통계 업데이트
      const processingTime = Date.now() - startTime;
      this.updatePerformanceStats(question, processingTime, reasoning.confidence);
      
      console.log(`✅ 질의 처리 완료 (${processingTime}ms, 신뢰도: ${Math.round(reasoning.confidence * 100)}%)`);
      
      return response;
      
    } catch (error) {
      console.error('❌ 질의 처리 실패:', error);
      
      // 오류 발생 시 기본 응답 반환
      const fallbackResponse: AdvancedRAGResponse = {
        answer: detectedLanguage === 'ko' 
          ? '죄송합니다. 질문을 처리하는 중에 오류가 발생했습니다. 다시 시도해주세요.'
          : '申し訳ございません。質問の処理中にエラーが発生しました。再度お試しください。',
        sources: [],
        language: detectedLanguage || 'ko',
        reasoning: {
          query: question,
          language: detectedLanguage || 'ko',
          steps: [],
          finalAnswer: '',
          confidence: 0.1,
          sources: [],
          processingTime: Date.now() - startTime,
          reasoning_chain: 'Error occurred during processing'
        },
        systemInfo: {
          processingTime: Date.now() - startTime,
          documentsScanned: 0,
          vectorSearchResults: 0,
          reasoningSteps: 0,
          confidence: 0.1
        }
      };
      
      return fallbackResponse;
    }
  }
  
  // 언어 감지
  private detectLanguage(text: string): 'ko' | 'ja' {
    const koreanChars = (text.match(/[가-힣ㄱ-ㅎㅏ-ㅣ]/g) || []).length;
    const japaneseChars = (text.match(/[ひらがなカタカナ一-龯]/g) || []).length;
    
    return koreanChars > japaneseChars ? 'ko' : 'ja';
  }
  
  // 언어 감지 신뢰도 계산
  private calculateLanguageConfidence(text: string, detectedLanguage: 'ko' | 'ja'): number {
    const totalChars = text.length;
    if (totalChars === 0) return 0;
    
    const koreanChars = (text.match(/[가-힣ㄱ-ㅎㅏ-ㅣ]/g) || []).length;
    const japaneseChars = (text.match(/[ひらがなカタカナ一-龯]/g) || []).length;
    
    const targetChars = detectedLanguage === 'ko' ? koreanChars : japaneseChars;
    const confidence = targetChars / totalChars;
    
    return Math.min(confidence * 2, 1.0); // 최대 1.0
  }
  
  // 고급 응답 생성
  private async generateAdvancedResponse(
    question: string,
    language: 'ko' | 'ja',
    vectorResults: VectorSearchResult[],
    reasoning: ChainOfThoughtResult,
    enableDebug: boolean
  ): Promise<AdvancedRAGResponse> {
    
    // 기본 RAG 응답 형식에 맞춰 변환
    const sources = vectorResults.map(result => ({
      chunk: {
        id: result.embedding.id,
        content: result.embedding.metadata.text,
        metadata: {
          source: result.embedding.metadata.source,
          language: result.embedding.metadata.language,
          section: result.embedding.metadata.section
        }
      },
      score: result.similarity
    }));
    
    // 시스템 정보
    const systemInfo = {
      processingTime: reasoning.processingTime,
      documentsScanned: this.systemStatus.totalDocuments,
      vectorSearchResults: vectorResults.length,
      reasoningSteps: reasoning.steps.length,
      confidence: reasoning.confidence
    };
    
    // 디버그 정보 (선택적)
    const debug = enableDebug ? {
      vectorSimilarities: vectorResults.map(r => r.similarity),
      keywordMatches: this.extractKeywords(question, language),
      languageDetection: { 
        detected: language, 
        confidence: this.calculateLanguageConfidence(question, language) 
      }
    } : undefined;
    
    const response: AdvancedRAGResponse = {
      answer: reasoning.finalAnswer,
      sources: sources,
      language: language,
      reasoning: reasoning,
      systemInfo: systemInfo,
      debug: debug
    };
    
    return response;
  }
  
  // 키워드 추출
  private extractKeywords(text: string, language: 'ko' | 'ja'): string[] {
    const stopWords = language === 'ko' 
      ? ['은', '는', '이', '가', '을', '를', '에', '에서', '로', '으로', '의', '와', '과']
      : ['は', 'が', 'を', 'に', 'で', 'から', 'まで', 'と', 'の', 'や'];
    
    return text.toLowerCase()
      .replace(/[^\p{L}\p{N}\s]/gu, ' ')
      .split(/\s+/)
      .filter(word => word.length > 1 && !stopWords.includes(word))
      .slice(0, 10); // 상위 10개
  }
  
  // 시스템 건강도 평가
  private assessSystemHealth(scanResult: ScanResult, dbStats: any): 'excellent' | 'good' | 'warning' | 'error' {
    if (scanResult.errors.length > 0) {
      return scanResult.errors.length > scanResult.processedFiles * 0.2 ? 'error' : 'warning';
    }
    
    if (scanResult.processedFiles === 0) {
      return 'error';
    }
    
    if (scanResult.processedFiles > 5 && dbStats.totalEmbeddings > 10) {
      return 'excellent';
    }
    
    return 'good';
  }
  
  // 성능 통계 업데이트
  private updatePerformanceStats(query: string, processingTime: number, accuracy: number): void {
    this.queryStats.push({
      query: query,
      time: processingTime,
      accuracy: accuracy,
      timestamp: new Date()
    });
    
    // 최근 100개 쿼리만 유지
    if (this.queryStats.length > 100) {
      this.queryStats = this.queryStats.slice(-100);
    }
    
    // 평균 성능 계산
    const totalQueries = this.queryStats.length;
    const avgTime = this.queryStats.reduce((sum, stat) => sum + stat.time, 0) / totalQueries;
    const avgAccuracy = this.queryStats.reduce((sum, stat) => sum + stat.accuracy, 0) / totalQueries;
    
    this.systemStatus.performance = {
      avgQueryTime: avgTime,
      avgAccuracy: avgAccuracy,
      totalQueries: this.systemStatus.performance.totalQueries + 1
    };
  }
  
  // 시스템 상태 조회
  getSystemStatus(): SystemStatus {
    return { ...this.systemStatus };
  }
  
  // 성능 통계 조회
  getPerformanceStats(): typeof this.queryStats {
    return [...this.queryStats];
  }
  
  // 시스템 재인덱싱
  async reindex(): Promise<void> {
    console.log('🔄 시스템 재인덱싱 시작...');
    await this.initialize(true);
    console.log('✅ 시스템 재인덱싱 완료');
  }
  
  // 시스템 진단
  async diagnose(): Promise<{
    status: string;
    issues: string[];
    recommendations: string[];
    performance: any;
  }> {
    const issues: string[] = [];
    const recommendations: string[] = [];
    
    // 문서 수 확인
    if (this.systemStatus.totalDocuments < 3) {
      issues.push('문서 수가 너무 적습니다');
      recommendations.push('더 많은 매뉴얼 문서를 추가하세요');
    }
    
    // 임베딩 수 확인
    if (this.systemStatus.totalEmbeddings < 10) {
      issues.push('벡터 임베딩 수가 부족합니다');
      recommendations.push('문서 청킹 전략을 개선하거나 문서를 추가하세요');
    }
    
    // 성능 확인
    if (this.systemStatus.performance.avgQueryTime > 5000) {
      issues.push('쿼리 처리 시간이 너무 깁니다');
      recommendations.push('벡터 인덱스 최적화를 고려하세요');
    }
    
    // 정확도 확인
    if (this.systemStatus.performance.avgAccuracy < 0.7) {
      issues.push('답변 정확도가 낮습니다');
      recommendations.push('문서 품질 개선이나 임베딩 모델 업그레이드를 고려하세요');
    }
    
    return {
      status: this.systemStatus.systemHealth,
      issues: issues,
      recommendations: recommendations,
      performance: this.systemStatus.performance
    };
  }
  
  // 유사한 질문 제안
  async suggestSimilarQueries(query: string, language: 'ko' | 'ja', limit: number = 5): Promise<string[]> {
    // 벡터 검색을 통해 유사한 콘텐츠 찾기
    const results = await this.vectorDB.search(query, language, limit * 2);
    
    // 각 문서에서 질문 형태의 문장 추출
    const suggestions: string[] = [];
    
    results.forEach(result => {
      const content = result.embedding.metadata.text;
      const questionPatterns = language === 'ko' 
        ? /[?？]|어떻게|무엇|왜|언제|어디서/g
        : /[?？]|どうやって|何|なぜ|いつ|どこで/g;
      
      const sentences = content.split(/[.。]/).filter(s => 
        s.length > 10 && s.length < 100 && questionPatterns.test(s)
      );
      
      suggestions.push(...sentences.slice(0, 2));
    });
    
    // 중복 제거 및 상위 결과 반환
    return Array.from(new Set(suggestions)).slice(0, limit);
  }
}

// 싱글톤 인스턴스
export const advancedRAGSystem = new AdvancedRAGSystem();