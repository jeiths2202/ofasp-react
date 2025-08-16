package com.openasp.cobol;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.WebSocket;
import java.net.http.WebSocket.Listener;
import java.nio.ByteBuffer;
import java.util.concurrent.CompletionStage;

/**
 * WebSocketServiceImpl - Implementation of WebSocketService
 * 
 * Handles WebSocket communication for display file processing
 * Implements position-based SMED rendering for interactive terminals
 */
public class WebSocketServiceImpl implements WebSocketService {
    
    private static final Logger logger = Logger.getLogger(WebSocketServiceImpl.class.getName());
    
    // WebSocket connection management
    private final Map<String, WebSocket> terminalConnections = new ConcurrentHashMap<>();
    private final Map<String, Consumer<Map<String, Object>>> updateHandlers = new ConcurrentHashMap<>();
    
    // Default WebSocket server endpoint
    private final String webSocketEndpoint = "ws://localhost:8080/smed/position";
    
    @Override
    public void enableInteractiveMode(String terminalId) throws Exception {
        logger.info("Enabling interactive mode for terminal: " + terminalId);
        
        if (terminalConnections.containsKey(terminalId)) {
            logger.info("Terminal already connected: " + terminalId);
            return;
        }
        
        try {
            // Create WebSocket connection
            URI uri = URI.create(webSocketEndpoint + "?terminal=" + terminalId);
            HttpClient client = HttpClient.newHttpClient();
            
            WebSocket webSocket = client.newWebSocketBuilder()
                .buildAsync(uri, new WebSocketListener(terminalId))
                .join();
            
            terminalConnections.put(terminalId, webSocket);
            
            // Send initialization message
            Map<String, Object> initMessage = new HashMap<>();
            initMessage.put("action", "initialize");
            initMessage.put("terminal_id", terminalId);
            initMessage.put("mode", "position_smed");
            
            sendMessage(webSocket, initMessage);
            
            logger.info("Interactive mode enabled for terminal: " + terminalId);
            
        } catch (Exception e) {
            logger.log(Level.SEVERE, "Failed to enable interactive mode", e);
            throw new Exception("Failed to connect WebSocket for terminal: " + terminalId, e);
        }
    }
    
    @Override
    public void disableInteractiveMode(String terminalId) throws Exception {
        logger.info("Disabling interactive mode for terminal: " + terminalId);
        
        WebSocket webSocket = terminalConnections.remove(terminalId);
        if (webSocket != null) {
            try {
                // Send cleanup message
                Map<String, Object> cleanupMessage = new HashMap<>();
                cleanupMessage.put("action", "cleanup");
                cleanupMessage.put("terminal_id", terminalId);
                
                sendMessage(webSocket, cleanupMessage);
                
                // Close connection
                webSocket.sendClose(WebSocket.NORMAL_CLOSURE, "Session ended").join();
                
            } catch (Exception e) {
                logger.log(Level.WARNING, "Error during WebSocket cleanup", e);
            }
        }
        
        logger.info("Interactive mode disabled for terminal: " + terminalId);
    }
    
    @Override
    public void sendPositionSmedDisplay(Map<String, Object> displayData) throws Exception {
        String terminalId = (String) displayData.get("terminal_id");
        
        if (terminalId == null) {
            throw new IllegalArgumentException("terminal_id is required in displayData");
        }
        
        logger.info("Sending position SMED display to terminal: " + terminalId);
        
        WebSocket webSocket = terminalConnections.get(terminalId);
        if (webSocket == null) {
            throw new IllegalStateException("Terminal not connected: " + terminalId);
        }
        
        // Prepare SMED display message
        Map<String, Object> smedMessage = new HashMap<>();
        smedMessage.put("action", "display_smed");
        smedMessage.put("terminal_id", terminalId);
        smedMessage.put("map_name", displayData.get("map_name"));
        smedMessage.put("map_data", displayData.get("map_data"));
        smedMessage.put("field_data", displayData.get("field_data"));
        smedMessage.put("timestamp", System.currentTimeMillis());
        
        sendMessage(webSocket, smedMessage);
        
        logger.fine("Position SMED display sent successfully");
    }
    
    @Override
    public void subscribeToPositionUpdates(String formatName, Consumer<Map<String, Object>> updateHandler) throws Exception {
        logger.info("Subscribing to position updates for format: " + formatName);
        
        updateHandlers.put(formatName, updateHandler);
        
        // Send subscription message to all connected terminals
        Map<String, Object> subscribeMessage = new HashMap<>();
        subscribeMessage.put("action", "subscribe_updates");
        subscribeMessage.put("format_name", formatName);
        
        for (WebSocket webSocket : terminalConnections.values()) {
            try {
                sendMessage(webSocket, subscribeMessage);
            } catch (Exception e) {
                logger.log(Level.WARNING, "Failed to send subscription message", e);
            }
        }
        
        logger.info("Subscribed to position updates for format: " + formatName);
    }
    
    @Override
    public void unsubscribeFromPositionUpdates(String formatName) throws Exception {
        logger.info("Unsubscribing from position updates for format: " + formatName);
        
        updateHandlers.remove(formatName);
        
        // Send unsubscription message to all connected terminals
        Map<String, Object> unsubscribeMessage = new HashMap<>();
        unsubscribeMessage.put("action", "unsubscribe_updates");
        unsubscribeMessage.put("format_name", formatName);
        
        for (WebSocket webSocket : terminalConnections.values()) {
            try {
                sendMessage(webSocket, unsubscribeMessage);
            } catch (Exception e) {
                logger.log(Level.WARNING, "Failed to send unsubscription message", e);
            }
        }
        
        logger.info("Unsubscribed from position updates for format: " + formatName);
    }
    
    @Override
    public void sendError(String terminalId, String message) throws Exception {
        logger.warning("Sending error to terminal " + terminalId + ": " + message);
        
        WebSocket webSocket = terminalConnections.get(terminalId);
        if (webSocket == null) {
            logger.warning("Cannot send error - terminal not connected: " + terminalId);
            return;
        }
        
        Map<String, Object> errorMessage = new HashMap<>();
        errorMessage.put("action", "error");
        errorMessage.put("terminal_id", terminalId);
        errorMessage.put("message", message);
        errorMessage.put("timestamp", System.currentTimeMillis());
        
        sendMessage(webSocket, errorMessage);
    }
    
    @Override
    public boolean isTerminalConnected(String terminalId) {
        WebSocket webSocket = terminalConnections.get(terminalId);
        return webSocket != null && !webSocket.isOutputClosed();
    }
    
    /**
     * Send JSON message via WebSocket
     */
    private void sendMessage(WebSocket webSocket, Map<String, Object> message) throws Exception {
        try {
            String json = mapToJson(message);
            webSocket.sendText(json, true).join();
        } catch (Exception e) {
            logger.log(Level.SEVERE, "Failed to send WebSocket message", e);
            throw new Exception("WebSocket send failed: " + e.getMessage(), e);
        }
    }
    
    /**
     * WebSocket Listener implementation
     */
    private class WebSocketListener implements WebSocket.Listener {
        private final String terminalId;
        
        public WebSocketListener(String terminalId) {
            this.terminalId = terminalId;
        }
        
        @Override
        public void onOpen(WebSocket webSocket) {
            logger.info("WebSocket opened for terminal: " + terminalId);
            WebSocket.Listener.super.onOpen(webSocket);
        }
        
        @Override
        public CompletionStage<?> onText(WebSocket webSocket, CharSequence data, boolean last) {
            try {
                if (last) {
                    String message = data.toString();
                    logger.fine("Received WebSocket message: " + message);
                    
                    // Parse and handle message
                    Map<String, Object> messageData = parseJsonToMap(message);
                    handleIncomingMessage(messageData);
                }
            } catch (Exception e) {
                logger.log(Level.WARNING, "Error processing WebSocket message", e);
            }
            
            return WebSocket.Listener.super.onText(webSocket, data, last);
        }
        
        @Override
        public CompletionStage<?> onClose(WebSocket webSocket, int statusCode, String reason) {
            logger.info("WebSocket closed for terminal " + terminalId + ": " + statusCode + " - " + reason);
            terminalConnections.remove(terminalId);
            return WebSocket.Listener.super.onClose(webSocket, statusCode, reason);
        }
        
        @Override
        public void onError(WebSocket webSocket, Throwable error) {
            logger.log(Level.SEVERE, "WebSocket error for terminal: " + terminalId, error);
            terminalConnections.remove(terminalId);
            WebSocket.Listener.super.onError(webSocket, error);
        }
    }
    
    /**
     * Handle incoming messages from WebSocket
     */
    private void handleIncomingMessage(Map<String, Object> messageData) {
        try {
            String action = (String) messageData.get("action");
            
            if ("position_update".equals(action)) {
                String formatName = (String) messageData.get("format_name");
                Consumer<Map<String, Object>> handler = updateHandlers.get(formatName);
                
                if (handler != null) {
                    handler.accept(messageData);
                }
            } else if ("ack".equals(action)) {
                logger.fine("Received acknowledgment: " + messageData);
            }
            
        } catch (Exception e) {
            logger.log(Level.WARNING, "Error handling incoming message", e);
        }
    }
    
    /**
     * Simple JSON to Map parser
     */
    private Map<String, Object> parseJsonToMap(String json) throws Exception {
        Map<String, Object> result = new HashMap<>();
        
        // Simple JSON parsing for basic key-value pairs
        json = json.trim();
        if (json.startsWith("{") && json.endsWith("}")) {
            json = json.substring(1, json.length() - 1);
            
            String[] pairs = json.split(",");
            for (String pair : pairs) {
                String[] kv = pair.split(":");
                if (kv.length == 2) {
                    String key = kv[0].trim().replaceAll("\"", "");
                    String value = kv[1].trim().replaceAll("\"", "");
                    result.put(key, value);
                }
            }
        }
        
        return result;
    }
    
    /**
     * Simple Map to JSON converter
     */
    private String mapToJson(Map<String, Object> map) {
        StringBuilder json = new StringBuilder("{");
        
        boolean first = true;
        for (Map.Entry<String, Object> entry : map.entrySet()) {
            if (!first) {
                json.append(",");
            }
            
            json.append("\"").append(entry.getKey()).append("\":");
            
            Object value = entry.getValue();
            if (value instanceof String) {
                json.append("\"").append(value).append("\"");
            } else if (value instanceof Number) {
                json.append(value);
            } else if (value instanceof Boolean) {
                json.append(value);
            } else if (value instanceof List) {
                json.append("[");
                List<?> list = (List<?>) value;
                for (int i = 0; i < list.size(); i++) {
                    if (i > 0) json.append(",");
                    if (list.get(i) instanceof String) {
                        json.append("\"").append(list.get(i)).append("\"");
                    } else {
                        json.append(list.get(i));
                    }
                }
                json.append("]");
            } else {
                json.append("\"").append(value != null ? value.toString() : "null").append("\"");
            }
            
            first = false;
        }
        
        json.append("}");
        return json.toString();
    }
}