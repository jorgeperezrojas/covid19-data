
const Code = require('@hapi/code');
const { DateTime } = require('luxon');
const Lab = require('@hapi/lab');

const { expect } = Code;
const lab = exports.lab = Lab.script();

const csvutil = require('./csvutil');

const CSV_CHECK_DEFAULT_CONFIG = {
	delimiter: ',',
	duplicates: false,
	emptyLines: true,
	emptyValues: false,
	encoding: 'utf8',
	limit: false,
	log: true
};
const FILES = {
	'confirmados.csv': CSV_CHECK_DEFAULT_CONFIG,
	'confirmados_comunas.csv': CSV_CHECK_DEFAULT_CONFIG,
	'confirmados_comunas_interpolado.csv': CSV_CHECK_DEFAULT_CONFIG,
	'encuesta_sochimi.csv': CSV_CHECK_DEFAULT_CONFIG,
	'muertes.csv': CSV_CHECK_DEFAULT_CONFIG,
	'notificaciones.csv': { ...CSV_CHECK_DEFAULT_CONFIG, emptyValues: true },
	'pacientes_en_uci.csv': CSV_CHECK_DEFAULT_CONFIG,
	'pcrs_region.csv': CSV_CHECK_DEFAULT_CONFIG,
	'resumen_nacional.csv': CSV_CHECK_DEFAULT_CONFIG,
};

// Check dates
csvutil(lab, FILES, csv => {
	csv.testDate('date year at least 2020', (date, rawDate) => expect(date.year, rawDate).to.to.be.at.least(2020));
	csv.testDate('date at most today', (date, rawDate) => expect(+date, rawDate).to.to.be.at.most(+DateTime.utc()));
});
