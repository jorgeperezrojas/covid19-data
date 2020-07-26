
const path = require('path');

const csvdata = require('csvdata');
const Enumerable = require('linq');

const { expect } = require('@hapi/code');
const { DateTime } = require('luxon');

const DATE_FORMAT = 'M/d/yyyy';

class CsvUtil
{
	constructor(lab, filename, config)
	{
		this.filename = filename;
		this.fullPath = path.join(__dirname, '../csv', filename);
		this.lab = lab;
		this.config = config;
	}

	async load() {
		if (this.data)
			return;
		this.data = await csvdata.load(this.fullPath);
	}

	check() {
		this.lab.test('File check: ' + this.filename, async() => {
			const ok = await csvdata.check(this.fullPath, this.config.check);
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

	testContiguousTotal(testName, run)
	{
		this.lab.test(testName, async () =>
		{
			await this.load();
			if (!this.data.length)
				return;

			const firstRow = this.data[0];
			const keys = Object.keys(firstRow);
			const validKeys = Enumerable
				.from(keys)
				.where(key => key.includes('/'));
			const pairs = validKeys
				.zip(validKeys.skip(1), (key1, key2) => ({ key1, key2 }))
				.toArray();
			for (const pair of pairs)
			{
				const totalValue1 = Enumerable
					.from(this.data)
					.select(row => row[pair.key1])
					.sum();
				const totalValue2 = Enumerable
					.from(this.data)
					.select(row => row[pair.key2])
					.sum();
				const info = `Columns: ${pair.key1} and ${pair.key2}`;
				run(totalValue1, totalValue2, info);
			}
		});
	}
}

module.exports = (lab, files, run, optionsOverride = {}) =>
{
	for (const filename in files)
	{
		const config = files[filename];
		const csv = new CsvUtil(lab, filename, config);
		lab.experiment(filename, () => {
			csv.check();
			run(csv);
		});
	}
};
