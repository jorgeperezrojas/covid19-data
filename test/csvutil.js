
const path = require('path');

const csvdata = require('csvdata');
const merge = require('merge-anything');

const { expect } = require('@hapi/code');
const { DateTime } = require('luxon');

const CSV_CHECK_OPTIONS = {
	delimiter: ',',
	duplicates: false,
	emptyLines: true,
	emptyValues: false,
	encoding: 'utf8',
	limit: false,
	log: true
};
const DATE_FORMAT = 'M/d/yyyy';

class CsvUtil
{
	constructor(lab, filename, checkConfig)
	{
		this.filename = filename;
		this.fullPath = path.join(__dirname, '../csv', filename);
		this.lab = lab;
		this.checkConfig = checkConfig;
	}

	async load() {
		if (this.data)
			return;
		this.data = await csvdata.load(this.fullPath);
	}

	check() {
		this.lab.test('File check: ' + this.filename, async() => {
			const ok = await csvdata.check(this.fullPath, this.checkConfig);
			expect(ok).to.be.true();
		});
	}

	testDate(testName, run)
	{
		this.lab.test(testName, async () =>
		{
			await this.load();
			for (const row of this.data)
			{
				for (const key of Object.keys(row))
				{
					if (!key.includes('/'))
						continue;

					const date = DateTime.fromFormat(key, DATE_FORMAT);
					await run(date, key);
				}
			}
		});
	}

	testValue(testName, run)
	{
		this.lab.test(testName, async () =>
		{
			await this.load();
			for (const row of this.data)
			{
				for (const key of Object.keys(row))
				{
					const value = row[key];
					// const metadata = `${JSON.stringify(row)}: ${key}`;
					await run(value, key);
				}
			}
		});
	}
}

module.exports = (lab, files, run, optionsOverride = {}) =>
{
	for (const filename of files)
	{
		const config = merge.merge(CSV_CHECK_OPTIONS, optionsOverride);
		const csv = new CsvUtil(lab, filename, config);
		lab.experiment(filename, () => {
			csv.check();
			run(csv);
		});
	}
};
