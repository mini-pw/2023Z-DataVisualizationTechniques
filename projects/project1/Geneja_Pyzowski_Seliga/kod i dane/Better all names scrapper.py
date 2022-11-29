"""
Script to get jumpers nationality and gender
"""
from typing import List


class Jumper:
	"""
	Class representing a ski jumper
	"""
	__slots__ = ['codex', 'name', 'nationality', 'gender']

	def __init__(self, codex: int, name: str = '', nationality: str = '', gender: str = ''):
		self.codex = codex
		self.name = name
		self.nationality = nationality
		self.gender = gender

	def __str__(self) -> str:
		return f'{self.name},{self.codex},{self.nationality},{self.gender}'

	def __repr__(self) -> str:
		return self.name

	def set_nationality(self, nationality: str):
		"""
		Method to set jumpers nationality
		:param nationality: Nationality to be set
		"""
		self.nationality = nationality

	def set_gender(self, gender: str):
		"""
		Method to set jumpers gender
		:param gender: Gender to be set
		"""
		self.gender = gender


def load_data() -> List[Jumper]:
	"""
	Function to load data from all_names.csv into python list of Jumpers
	:return: List of Jumpers
	"""
	jumpers = []
	with open('Data/all_names.csv') as f:
		f.readline()  # Line with column headers
		for line in f.readlines():
			line = line.strip()
			if line[-1] == ',':
				continue
			try:
				name, codex = line.split(',')
				jumper = Jumper(int(float(codex)), name)
			except Exception:
				print(line)
				continue
			jumpers.append(jumper)

	return jumpers


def load_website() -> dict:
	"""
	Function to load data from FIS website

	:return: List of jumpers from website
	"""
	with open('Data/fis_web.txt') as f:
		data = ''.join(f.readlines())
	t = data.split('<a class="table-row" href="https://www.fis-ski.com/DB/general/athlete-biography.html?sectorcod')[1:]
	ids = [s.split('<div class="g-lg g-md g-sm-24 g-xs-24 justify-left">')[2].split('</div>')[0] for s in t]
	names = [s.split(' g-sm g-xs justify-left flex-sm-wrap flex-xs-wrap">')[1].split('</div>')[0] for s in t]
	countries = [s.split('<span class="country__name-short">')[1].split('</span>')[0] for s in t]
	genders = [s.split('bold">')[1].split('</div>')[0] for s in t]
	j = {int(codex): [name, country, gender] for codex, name, country, gender in zip(ids, names, countries, genders)}
	return j


def merge_data(jumpers: List[Jumper], website: dict) -> List[Jumper]:
	"""
	Function to merge
	:param jumpers:
	:param website:
	:return:
	"""
	new_jumpers = []
	for j in jumpers:
		data = website.pop(j.codex, None)
		if not data:
			continue
		j.set_nationality(data[1])
		j.set_gender(data[2])
		new_jumpers.append(j)

	return new_jumpers


def save_to_csv(jumpers: List[Jumper]):
	"""
	Function to write list of jumpers to improved_names.csv
	"""
	with open('Data/improved_names.csv', 'w') as f:
		f.write('name,codex,nationality,gender\n')
		j = '\n'.join([str(j) for j in jumpers])
		f.write(j)


def main() -> None:
	"""
	Main function of the script
	"""
	jumpers = load_data()
	website = load_website()
	jumpers = merge_data(jumpers, website)
	save_to_csv(jumpers)


if __name__ == '__main__':
	main()
