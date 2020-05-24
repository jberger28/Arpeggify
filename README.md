## Arpeggify ##

Arpeggify generates a .wav file that arpeggiates the tune specified in an Arpeggify program. Capable of arpeggiating any  Major7, Minor7, and Dominant 7 chords, Arpeggify can generate infinitely many pieces of music, which might be used to assist with composition or as backing tracks for practicing jazz improvization.:

Example Program 1 (Basic C blues):

	To run from the 'project' directory type:
	dotnet run ../../examples/example-1.arp example-1.wav

	To run from the 'lang' directory type:
	dotnet run --project project/project.fsproj ../examples/example-1.arp example-1.wav

	A file 'example-1.wav' will be generated in the the current directory

Example Program 2 (John Coltrane's Giant Steps):

	To run from the 'project' directory type:
	dotnet run ../../examples/example-2.arp example-2.wav

	To run from the 'lang' directory type:
	dotnet run --project project/project.fsproj ../examples/example-2.arp example-2.wav

	A file 'example-2.wav' will be generated in the current directory.

Example Program 3 (The jazz standard "I've Never Been in Love Before"):

	To run from the 'project' directory type:
	dotnet run ../../examples/example-3.arp example-3.wav

	To run from the 'lang' directory type:
	dotnet run --project project/project.fsproj ../examples/example-3.arp example-3.wav

	A file 'example-3.wav' will be generated in the current directory.

** NOTE: Any of the above example programs may be given an additional command-line argument of a positive integer to arpeggiate at a tempo other than the default value of 200 beats per minute


   	 For example, to run 'example-1' from the 'project' directory but at 300 BPM type:
	 dotnet run ../../examples/example-1.arp example-1.wav 300

Test Suite:

	To run the tests from either the 'lang' directory or the 'tests' directory type:
	dotnet test

	