// 실제 TensorFlow.js 기반 벡터 데이터베이스
import * as tf from '@tensorflow/tfjs';
import '@tensorflow/tfjs-backend-cpu';
import '@tensorflow/tfjs-backend-webgl';
import * as use from '@tensorflow-models/universal-sentence-encoder';
import { DocumentChunk } from './ragService';

// 실제 벡터 임베딩 인터페이스
export interface RealVectorEmbedding {
  id: string;
  vector: Float32Array;
  metadata: {
    documentId: string;
    chunkIndex: number;
    text: string;
    language: 'ko' | 'ja';
    source: string;
    section?: string;
    timestamp: Date;
  };
}

// 실제 검색 결과 인터페이스
export interface RealVectorSearchResult {
  embedding: RealVectorEmbedding;
  similarity: number;
  distance: number;
  chunk: DocumentChunk;
  score: number;
}

// 실제 TensorFlow.js 기반 벡터 데이터베이스
export class RealVectorDatabase {
  private model: use.UniversalSentenceEncoder | null = null;
  private embeddings: Map<string, RealVectorEmbedding> = new Map();
  private indexByLanguage: Map<string, RealVectorEmbedding[]> = new Map();
  private isInitialized = false;
  private isModelLoaded = false;
  private isDataLoaded = false;
  private lastLoadTime: Date | null = null;

  // 모델 초기화 (캐싱 지원)
  async initialize(): Promise<void> {
    // 이미 모델이 로드되어 있으면 건너뛰기
    if (this.isModelLoaded && this.model) {
      console.log('📦 TensorFlow.js 모델이 이미 캐시됨 - 재사용');
      this.isInitialized = true;
      return;
    }

    console.log('🤖 TensorFlow.js 백엔드 초기화 중...');
    
    try {
      // TensorFlow.js 백엔드 준비 확인
      await tf.ready();
      console.log('✅ TensorFlow.js 백엔드 준비됨:', tf.getBackend());
      
      console.log('🤖 TensorFlow.js Universal Sentence Encoder 모델 로딩 중...');
      const startTime = Date.now();

      // 실제 Universal Sentence Encoder 모델 로드
      this.model = await use.load();
      
      const loadTime = Date.now() - startTime;
      console.log(`✅ 실제 벡터 임베딩 모델 로드 완료 (${loadTime}ms)`);
      console.log(`📐 모델 차원: 512 (Universal Sentence Encoder)`);
      console.log(`🖥️ 사용 중인 백엔드: ${tf.getBackend()}`);
      
      this.isModelLoaded = true;
      this.isInitialized = true;
    } catch (error) {
      console.error('❌ TensorFlow.js 모델 로드 실패:', error);
      console.error('💡 사용 가능한 백엔드:', tf.engine().backendNames);
      throw new Error('실제 벡터 임베딩 모델을 로드할 수 없습니다');
    }
  }

  // 실제 문서 추가 및 벡터화 (중복 방지)
  async addDocument(document: DocumentChunk): Promise<void> {
    if (!this.model) {
      throw new Error('벡터 데이터베이스가 초기화되지 않았습니다');
    }

    // 이미 처리된 문서인지 확인
    const docKey = `${document.id}_${document.metadata.source}`;
    const existingDoc = Array.from(this.embeddings.values()).find(
      emb => emb.metadata.documentId === document.id && emb.metadata.source === document.metadata.source
    );
    
    if (existingDoc) {
      console.log(`📦 문서가 이미 벡터 DB에 캐시됨: ${document.metadata.source}`);
      return;
    }

    console.log(`🔄 실제 벡터 임베딩 생성: ${document.metadata.source}`);
    const startTime = Date.now();

    try {
      // 문서를 실제 청크로 분할
      const chunks = this.chunkDocument(document);
      console.log(`📄 문서를 ${chunks.length}개 청크로 분할`);

      for (let i = 0; i < chunks.length; i++) {
        const chunk = chunks[i];
        const embeddingId = `${document.id}_chunk_${i}`;

        console.log(`🧮 청크 ${i + 1}/${chunks.length} 벡터 임베딩 생성 중...`);
        
        // 실제 Universal Sentence Encoder로 임베딩 생성
        const embeddings = await this.model.embed([chunk]);
        const embeddingArray = await embeddings.data();
        embeddings.dispose(); // 메모리 해제

        // Float32Array로 변환
        const vector = new Float32Array(embeddingArray);

        const embedding: RealVectorEmbedding = {
          id: embeddingId,
          vector: vector,
          metadata: {
            documentId: document.id,
            chunkIndex: i,
            text: chunk,
            language: document.metadata.language,
            source: document.metadata.source,
            section: document.metadata.section,
            timestamp: new Date()
          }
        };

        this.embeddings.set(embeddingId, embedding);

        // 언어별 인덱스 구축
        const langKey = document.metadata.language;
        if (!this.indexByLanguage.has(langKey)) {
          this.indexByLanguage.set(langKey, []);
        }
        this.indexByLanguage.get(langKey)!.push(embedding);

        console.log(`✅ 청크 ${i + 1} 벡터 임베딩 완료 (차원: ${vector.length})`);
      }

      const processingTime = Date.now() - startTime;
      console.log(`🎯 문서 벡터화 완료: ${document.metadata.source} (${processingTime}ms)`);
      
      // 데이터 로드 상태 업데이트
      this.isDataLoaded = true;
      this.lastLoadTime = new Date();

    } catch (error) {
      console.error(`❌ 문서 벡터화 실패: ${document.metadata.source}`, error);
      throw error;
    }
  }

  // 실제 유사도 검색
  async search(query: string, language: 'ko' | 'ja', limit: number = 10): Promise<RealVectorSearchResult[]> {
    if (!this.model) {
      throw new Error('벡터 데이터베이스가 초기화되지 않았습니다');
    }

    console.log(`🔍 실제 벡터 검색 수행: "${query}" (${language})`);
    const startTime = Date.now();

    try {
      // 쿼리를 실제 벡터로 변환
      const queryEmbeddings = await this.model.embed([query]);
      const queryVector = new Float32Array(await queryEmbeddings.data());
      queryEmbeddings.dispose();

      const results: RealVectorSearchResult[] = [];

      // 모든 임베딩과 실제 코사인 유사도 계산
      this.embeddings.forEach((embedding) => {
        const similarity = this.calculateCosineSimilarity(queryVector, embedding.vector);
        
        if (similarity > 0.1) { // 최소 임계값
          results.push({
            embedding,
            similarity,
            distance: 1 - similarity,
            chunk: {
              id: embedding.metadata.documentId,
              content: embedding.metadata.text,
              metadata: {
                source: embedding.metadata.source,
                language: embedding.metadata.language,
                section: embedding.metadata.section
              }
            },
            score: similarity
          });
        }
      });

      // 유사도 기준 정렬
      results.sort((a, b) => b.similarity - a.similarity);

      const searchTime = Date.now() - startTime;
      console.log(`🎯 벡터 검색 완료: ${results.length}개 결과 (${searchTime}ms)`);

      return results.slice(0, limit);

    } catch (error) {
      console.error('❌ 벡터 검색 실패:', error);
      throw error;
    }
  }

  // 실제 코사인 유사도 계산
  private calculateCosineSimilarity(vector1: Float32Array, vector2: Float32Array): number {
    if (vector1.length !== vector2.length) {
      throw new Error('벡터 차원이 일치하지 않습니다');
    }

    let dotProduct = 0;
    let magnitude1 = 0;
    let magnitude2 = 0;

    for (let i = 0; i < vector1.length; i++) {
      dotProduct += vector1[i] * vector2[i];
      magnitude1 += vector1[i] * vector1[i];
      magnitude2 += vector2[i] * vector2[i];
    }

    magnitude1 = Math.sqrt(magnitude1);
    magnitude2 = Math.sqrt(magnitude2);

    if (magnitude1 === 0 || magnitude2 === 0) {
      return 0;
    }

    return dotProduct / (magnitude1 * magnitude2);
  }

  // 문서 청킹 (실제 의미 단위 분할)
  private chunkDocument(document: DocumentChunk): string[] {
    const content = document.content;
    const chunks: string[] = [];
    
    // 문장 단위로 분할
    const sentences = content.split(/[.!?。！？]/);
    
    let currentChunk = '';
    const maxChunkLength = 500; // 토큰 수 제한
    
    for (const sentence of sentences) {
      const trimmedSentence = sentence.trim();
      if (trimmedSentence.length === 0) continue;
      
      if (currentChunk.length + trimmedSentence.length > maxChunkLength) {
        if (currentChunk.length > 0) {
          chunks.push(currentChunk.trim());
          currentChunk = trimmedSentence;
        } else {
          chunks.push(trimmedSentence);
        }
      } else {
        currentChunk += (currentChunk.length > 0 ? '. ' : '') + trimmedSentence;
      }
    }
    
    if (currentChunk.length > 0) {
      chunks.push(currentChunk.trim());
    }
    
    return chunks.length > 0 ? chunks : [content];
  }

  // 데이터베이스 초기화
  clear(): void {
    console.log('🗑️ 실제 벡터 데이터베이스 초기화');
    this.embeddings.clear();
    this.indexByLanguage.clear();
  }

  // 통계 정보
  getStats(): { totalEmbeddings: number; byLanguage: Record<string, number> } {
    const stats = {
      totalEmbeddings: this.embeddings.size,
      byLanguage: {} as Record<string, number>
    };

    this.indexByLanguage.forEach((embeddings, language) => {
      stats.byLanguage[language] = embeddings.length;
    });

    return stats;
  }

  // 시스템 상태 확인
  isReady(): boolean {
    return this.isInitialized && this.model !== null;
  }
  
  // 데이터 로드 상태 확인
  hasData(): boolean {
    return this.isDataLoaded && this.embeddings.size > 0;
  }
  
  // 캐시 상태 정보
  getCacheStatus(): {
    modelCached: boolean;
    dataCached: boolean;
    lastLoadTime: Date | null;
    totalDocuments: number;
  } {
    return {
      modelCached: this.isModelLoaded,
      dataCached: this.isDataLoaded,
      lastLoadTime: this.lastLoadTime,
      totalDocuments: this.embeddings.size
    };
  }

  // 메모리 사용량 정보
  getMemoryUsage(): { embeddingCount: number; estimatedMB: number } {
    const embeddingCount = this.embeddings.size;
    // 각 임베딩은 대략 512 float32 = 2KB + 메타데이터
    const estimatedMB = (embeddingCount * 3) / 1024; // 대략 3KB per embedding
    
    return { embeddingCount, estimatedMB };
  }

  // 디버그: 모든 저장된 임베딩 정보 출력
  debugPrintAllEmbeddings(): void {
    console.log('=== 실제 벡터 DB 디버그 정보 ===');
    console.log(`총 임베딩 수: ${this.embeddings.size}`);
    
    this.indexByLanguage.forEach((embeddings, language) => {
      console.log(`${language}: ${embeddings.length}개 임베딩`);
      
      if (embeddings.length > 0) {
        console.log(`  - 첫 번째 임베딩 차원: ${embeddings[0].vector.length}`);
        console.log(`  - 벡터 타입: ${embeddings[0].vector.constructor.name}`);
      }
    });

    if (this.embeddings.size > 0) {
      const firstEmbedding = Array.from(this.embeddings.values())[0];
      console.log('\n=== 샘플 임베딩 정보 ===');
      console.log(`소스: ${firstEmbedding.metadata.source}`);
      console.log(`텍스트: ${firstEmbedding.metadata.text.substring(0, 100)}...`);
      console.log(`벡터 차원: ${firstEmbedding.vector.length}`);
      console.log(`벡터 샘플: [${Array.from(firstEmbedding.vector.slice(0, 5)).map(v => v.toFixed(4)).join(', ')}...]`);
    }
  }
}

// 싱글톤 인스턴스
export const realVectorDatabase = new RealVectorDatabase();