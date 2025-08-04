/**
 * Centralized catalog.json configuration for Node.js
 * Environment variable based configuration - no hardcoding
 */

const path = require('path');

// Environment-based configuration
const CONFIG_ROOT = process.env.OPENAP_CONFIG_ROOT || '/home/aspuser/app/config';
const CATALOG_FILENAME = process.env.CATALOG_FILENAME || 'catalog.json';

/**
 * Get catalog.json path from environment variables
 * @returns {string} Absolute path to the centralized catalog.json
 */
function getCatalogPath() {
    return path.join(CONFIG_ROOT, CATALOG_FILENAME);
}

/**
 * Get configuration root directory from environment
 * @returns {string} Configuration root directory path
 */
function getConfigRoot() {
    return CONFIG_ROOT;
}

module.exports = {
    getCatalogPath,
    getConfigRoot,
    CATALOG_FILE: getCatalogPath()
};