
const Code = require('@hapi/code');
const { DateTime } = require('luxon');
const Lab = require('@hapi/lab');

const { expect } = Code;
const lab = exports.lab = Lab.script();

const csvutil = require('./csvutil');

const FILES = [
	'confirmados.csv',
	'confirmados_comunas.csv',
	'confirmados_comunas_interpolado.csv',
	'encuesta_sochimi.csv',
	'muertes.csv',
	'notificaciones.csv',
	'pacientes_en_uci.csv',
	'pcrs_region.csv',
	'resumen_nacional.csv'
];

// Check dates
csvutil(lab, FILES, csv => {
	csv.testDate('date year at least 2020', (date, rawDate) => expect(date.year, rawDate).to.to.be.at.least(2020));
	csv.testDate('date at most today', (date, rawDate) => expect(+date, rawDate).to.to.be.at.most(+DateTime.utc()));
});

// Check specific files that must have values
csvutil(lab, ['notificaciones.csv'], () => {}, { emptyValues: true });
