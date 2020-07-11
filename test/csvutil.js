
const path = require('path');

const csvdata = require('csvdata');

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
	constructor(lab, filename)
	{
		this.filename = filename;
		this.fullPath = path.join(__dirname, '../csv', filename);
		this.lab = lab;
	}

	async load() {
		if (this.data)
			return;
		this.data = await csvdata.load(this.fullPath);
	}

	check() {
		this.lab.test('File check: ' + this.filename, async() => {
			const ok = await csvdata.check(this.fullPath, CSV_CHECK_OPTIONS);
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
}

module.exports = (lab, files, run) =>
{
	for (const filename of files)
	{
		let csv = new CsvUtil(lab, filename);
		lab.experiment(filename, () => {
			csv.check();
			run(csv);
		});
	}
};
