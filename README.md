# OpenASP System

OpenASP(Open Advanced System Products) 시스템입니다.

## 주요 기능

- **SMED 맵 기반 인증 플로우**: 터미널 스타일 UI를 통한 사용자 인증
- **OpenASP Manager**: 웹 기반 관리자 인터페이스
- **멀티 타입 프로그램 지원**: JAVA, COBOL, SHELL 프로그램 실행
- **실시간 시스템 모니터링**: CPU, 메모리, 디스크 사용량 모니터링

## 시스템 구성

### 1. OpenASP SMED 웹 인터페이스 (포트 3000)
- React + TypeScript 기반
- SMED 맵 기반 터미널 스타일 UI
- 사용자 인증 및 프로그램 실행

### 2. OpenASP Manager (포트 3007)
- 웹 기반 관리자 인터페이스
- 대시보드 및 시스템 모니터링
- SMED 맵 관리 및 설정

### 3. OpenASP API Server (포트 8000)
- Flask 기반 API 서버
- 멀티 타입 프로그램 실행 지원
- 실시간 시스템 정보 제공

## 기술 스택

- **Frontend**: React 19, TypeScript, Tailwind CSS
- **Backend**: Flask (Python), psutil
- **Database**: JSON 기반 설정 파일
- **Program Support**: Java, COBOL, Shell Script

---

*이 프로젝트는 기존 Create React App을 기반으로 구축되었습니다.*

## Available Scripts

In the project directory, you can run:

### `npm start`

Runs the app in the development mode.\
Open [http://localhost:3000](http://localhost:3000) to view it in the browser.

The page will reload if you make edits.\
You will also see any lint errors in the console.

### `npm test`

Launches the test runner in the interactive watch mode.\
See the section about [running tests](https://facebook.github.io/create-react-app/docs/running-tests) for more information.

### `npm run build`

Builds the app for production to the `build` folder.\
It correctly bundles React in production mode and optimizes the build for the best performance.

The build is minified and the filenames include the hashes.\
Your app is ready to be deployed!

See the section about [deployment](https://facebook.github.io/create-react-app/docs/deployment) for more information.

### `npm run eject`

**Note: this is a one-way operation. Once you `eject`, you can’t go back!**

If you aren’t satisfied with the build tool and configuration choices, you can `eject` at any time. This command will remove the single build dependency from your project.

Instead, it will copy all the configuration files and the transitive dependencies (webpack, Babel, ESLint, etc) right into your project so you have full control over them. All of the commands except `eject` will still work, but they will point to the copied scripts so you can tweak them. At this point you’re on your own.

You don’t have to ever use `eject`. The curated feature set is suitable for small and middle deployments, and you shouldn’t feel obligated to use this feature. However we understand that this tool wouldn’t be useful if you couldn’t customize it when you are ready for it.

## Learn More

You can learn more in the [Create React App documentation](https://facebook.github.io/create-react-app/docs/getting-started).

To learn React, check out the [React documentation](https://reactjs.org/).
