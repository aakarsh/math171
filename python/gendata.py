ptime_names = ["s ahmed", "s arabhi", "m bodas", "s desousa", "c fan", "t fish", "j hilliard", "t huynh", "k jensen", "a jiru", "j jordan", "o kovaleva", "r low",    "j lum", "w newball", "a nguyen", "t nguyen",  "v nguyen", "l papay", "l roper", "l sega", "t smith", "a strong", "a talebi", "p tanniru", "a tran",   "q tran", "j trubey", "m van-der-poel", "m vartanian", "s vergara", "j wang", "e zabric", "m zoubeidi", "p rogers"]

pfac_names = ["j becker", "m blockus", "m beeson", "r dodd", "l foster", "d goldston", "t hsu", "k kellum", "r kubelka", "h ng", "s obaid", "b pence", "b peterson", "r pfiefer", "f rivera", "m saleem", "e schmeichel", "w so", "m stanley", "c roddick", "sliva-spitzer",  "h katsuura",  "m beeson"]

afac_names =  ["s crunk", "a gottlieb", "p koev", "b lee", "j maruskin", "s simic", "m cayco-gajic"]

class Session:
	day = ""
	def __init__(self, day, start, end):
		self.day = day
		self.start = start
		self.end = end
	def intersects(self, other):
		if self.day != other.day: return 0
		if other.start <= self.start and self.start <= other.end: return 1
		if other.start <= self.end and self.end <= other.end: return 1
		return 0
	def __repr__(self):
		return "%s %04d-%04d" %(self.day.title(), self.start, self.end)

class Person:
	name = ""
	pos = ""
	def __init__(self, name, pos):
		self.sessions = []
		self.name = name
		self.pos = pos
		if " " in self.name:
			self.name = self.name[2:] + " " + self.name[:1]
	def __repr__(self):
		return self.name.title() + " (" + self.pos + ")"
	def addClass(self, days, time):
		t_start = int(time[0:4])
		t_end = int(time[5:9])
		for d in days:
			self.sessions.append(Session(d, t_start, t_end))
	def canWatch(self, other):
		for other_class in other.sessions:
			# Check if this conflicts with any of my classes
			for my_class in self.sessions:
				if my_class.intersects(other_class):
					break
			# Doesn't conflict, good
			else:
				return other_class
		else:
			return False


ptime = {}
pfac = {}
afac = {}

for ptime_name in ptime_names: ptime[ptime_name] = Person(ptime_name, "PT")
for pfac_name in pfac_names: pfac[pfac_name] = Person(pfac_name, "Prof")
for afac_name in afac_names: afac[afac_name] = Person(afac_name, "Asst")

f = open("data.txt", "r")
for line in f:
	stuff = eval("[" + line + "]")
	name = stuff[2]
	if ptime.has_key(name):
		ptime[name].addClass(stuff[0], stuff[1])
	elif pfac.has_key(name):
		pfac[name].addClass(stuff[0], stuff[1])
	elif afac.has_key(name):
		afac[name].addClass(stuff[0], stuff[1])
f.close()

ptime = ptime.values()
pfac = pfac.values()
afac = afac.values()
