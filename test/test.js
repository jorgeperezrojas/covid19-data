
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
	'confirmados.csv': { check: CSV_CHECK_DEFAULT_CONFIG, cumulative: true },
	'confirmados_comunas.csv': { check: CSV_CHECK_DEFAULT_CONFIG },
	'confirmados_comunas_interpolado.csv': { check: CSV_CHECK_DEFAULT_CONFIG },
	'encuesta_sochimi.csv': { check: CSV_CHECK_DEFAULT_CONFIG },
	'muertes.csv': { check: CSV_CHECK_DEFAULT_CONFIG, cumulative: true },
	'notificaciones.csv': { check: { ...CSV_CHECK_DEFAULT_CONFIG, emptyValues: true }, cumulative: true },
	'pacientes_en_uci.csv': { check: CSV_CHECK_DEFAULT_CONFIG },
	'pcrs_region.csv': { check: CSV_CHECK_DEFAULT_CONFIG },
	'resumen_nacional.csv': { check: CSV_CHECK_DEFAULT_CONFIG },
};

// Check dates
csvutil(lab, FILES, csv => {
	csv.testDate('date year at least 2020', (date, rawDate) => expect(date.year, rawDate).to.to.be.at.least(2020));
	csv.testDate('date at most today', (date, rawDate) => expect(+date, rawDate).to.to.be.at.most(+DateTime.utc()));
	if (csv.config.cumulative)
		csv.testContiguousTotal('fail', (firstValue, secondValue, info) => expect(secondValue, info).to.be.at.least(firstValue));
});
