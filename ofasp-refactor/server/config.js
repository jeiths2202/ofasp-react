/**
 * Server Configuration
 * Configuration for all server-side components
 */

require('dotenv').config();

const SERVER_CONFIG = {
  FILE_SERVER: {
    PORT: process.env.FILE_SERVER_PORT || 3006,
    HOST: process.env.FILE_SERVER_HOST || 'localhost'
  },
  PDF_CONVERTER: {
    PORT: process.env.PDF_CONVERTER_PORT || 3001,
    HOST: process.env.PDF_CONVERTER_HOST || 'localhost'
  },
  ASP_MANAGER: {
    PORT: process.env.ASP_MANAGER_PORT || 3007,
    HOST: process.env.ASP_MANAGER_HOST || 'localhost'
  },
  SMED_INTERFACE: {
    PORT: process.env.SMED_INTERFACE_PORT || 3000,
    HOST: process.env.SMED_INTERFACE_HOST || 'localhost'
  }
};

const PATHS = {
  ASP_MANUALS: process.env.ASP_MANUALS_PATH || '/data/asp-manuals',
  TEMP_CONVERSION: process.env.TEMP_CONVERSION_PATH || '/tmp/pdf_conversion'
};

const CORS_CONFIG = {
  ALLOWED_ORIGINS: [
    process.env.REACT_APP_BASE_URL || 'http://localhost:3005',
    process.env.REACT_APP_SMED_URL || 'http://localhost:3000',
    process.env.REACT_APP_ASP_MANAGER_URL || 'http://localhost:3007'
  ],
  CREDENTIALS: true
};

module.exports = {
  SERVER_CONFIG,
  PATHS,
  CORS_CONFIG
};