package com.openasp.logging;

import java.io.*;
import java.net.HttpURLConnection;
import java.net.URL;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.Map;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * LogClient - Centralized logging client for OpenASP Java programs
 * Sends logs to the API server for display in OpenASP Manager Log Management page
 */
public class LogClient {
    private static final String LOG_API_URL = "http://localhost:8000/api/logs";
    private static final ObjectMapper objectMapper = new ObjectMapper();
    private static LogClient instance;
    
    private String programName;
    private String programType;
    private String packageName;
    private String className;
    
    private LogClient() {
        // Private constructor for singleton
    }
    
    public static synchronized LogClient getInstance() {
        if (instance == null) {
            instance = new LogClient();
        }
        return instance;
    }
    
    /**
     * Initialize the log client with program information
     */
    public void initialize(String programName, String programType, String packageName, String className) {
        this.programName = programName;
        this.programType = programType;
        this.packageName = packageName;
        this.className = className;
    }
    
    /**
     * Log INFO level message
     */
    public void info(String message) {
        sendLog("INFO", message, null);
    }
    
    /**
     * Log INFO level message with additional details
     */
    public void info(String message, Map<String, Object> details) {
        sendLog("INFO", message, details);
    }
    
    /**
     * Log DEBUG level message
     */
    public void debug(String message) {
        sendLog("DEBUG", message, null);
    }
    
    /**
     * Log DEBUG level message with additional details
     */
    public void debug(String message, Map<String, Object> details) {
        sendLog("DEBUG", message, details);
    }
    
    /**
     * Log WARNING level message
     */
    public void warning(String message) {
        sendLog("WARNING", message, null);
    }
    
    /**
     * Log WARNING level message with additional details
     */
    public void warning(String message, Map<String, Object> details) {
        sendLog("WARNING", message, details);
    }
    
    /**
     * Log ERROR level message
     */
    public void error(String message) {
        sendLog("ERROR", message, null);
    }
    
    /**
     * Log ERROR level message with additional details
     */
    public void error(String message, Map<String, Object> details) {
        sendLog("ERROR", message, details);
    }
    
    /**
     * Log ERROR level message with exception
     */
    public void error(String message, Exception e) {
        Map<String, Object> details = new HashMap<>();
        details.put("exception", e.getClass().getSimpleName());
        details.put("exception_message", e.getMessage());
        sendLog("ERROR", message, details);
    }
    
    /**
     * Log SMED MAP related operations
     */
    public void smedInfo(String message, String mapName) {
        Map<String, Object> details = new HashMap<>();
        details.put("map_name", mapName);
        details.put("operation_type", "SMED_MAP");
        sendLog("INFO", message, details);
    }
    
    /**
     * Send log entry to the API server
     */
    private void sendLog(String level, String message, Map<String, Object> additionalDetails) {
        try {
            Map<String, Object> logData = new HashMap<>();
            logData.put("timestamp", LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")));
            logData.put("level", level);
            logData.put("source", "java-program");
            logData.put("message", message);
            
            // Build details map
            Map<String, Object> details = new HashMap<>();
            if (programName != null) details.put("program_name", programName);
            if (programType != null) details.put("program_type", programType);
            if (packageName != null) details.put("package_name", packageName);
            if (className != null) details.put("class_name", className);
            
            // Add additional details if provided
            if (additionalDetails != null) {
                details.putAll(additionalDetails);
            }
            
            if (!details.isEmpty()) {
                logData.put("details", details);
            }
            
            // Send HTTP POST request
            URL url = new URL(LOG_API_URL);
            HttpURLConnection conn = (HttpURLConnection) url.openConnection();
            conn.setRequestMethod("POST");
            conn.setRequestProperty("Content-Type", "application/json");
            conn.setDoOutput(true);
            
            String jsonData = objectMapper.writeValueAsString(logData);
            
            try (OutputStream os = conn.getOutputStream()) {
                byte[] input = jsonData.getBytes("utf-8");
                os.write(input, 0, input.length);
            }
            
            int responseCode = conn.getResponseCode();
            if (responseCode == 200) {
                // Success - also print to console for backward compatibility
                System.err.println("[" + level + "] " + message);
            } else {
                // Fallback to console if API is not available
                System.err.println("[" + level + "] " + message + " (API unavailable: " + responseCode + ")");
            }
            
            conn.disconnect();
            
        } catch (Exception e) {
            // Fallback to console if logging fails
            System.err.println("[" + level + "] " + message + " (Log API error: " + e.getMessage() + ")");
        }
    }
    
    /**
     * Create a convenience logger for a specific class
     */
    public static Logger getLogger(Class<?> clazz) {
        LogClient client = getInstance();
        client.initialize(
            clazz.getSimpleName(),
            "JAVA",
            clazz.getPackage() != null ? clazz.getPackage().getName() : "default",
            clazz.getSimpleName()
        );
        return new Logger(client);
    }
    
    /**
     * Logger wrapper class for easier usage
     */
    public static class Logger {
        private final LogClient client;
        
        private Logger(LogClient client) {
            this.client = client;
        }
        
        public void info(String message) {
            client.info(message);
        }
        
        public void info(String message, Map<String, Object> details) {
            client.info(message, details);
        }
        
        public void debug(String message) {
            client.debug(message);
        }
        
        public void debug(String message, Map<String, Object> details) {
            client.debug(message, details);
        }
        
        public void warning(String message) {
            client.warning(message);
        }
        
        public void warning(String message, Map<String, Object> details) {
            client.warning(message, details);
        }
        
        public void error(String message) {
            client.error(message);
        }
        
        public void error(String message, Map<String, Object> details) {
            client.error(message, details);
        }
        
        public void error(String message, Exception e) {
            client.error(message, e);
        }
        
        public void smedInfo(String message, String mapName) {
            client.smedInfo(message, mapName);
        }
    }
}