#!/usr/bin/env perl
use warnings;
use strict;
use Pod::Usage;
use Getopt::Long;

our $REVISION = '$Revision: a0607e6e595960936a1970db4668fa9fea646533 $';
our $DATE =	'$Date: 2020-06-07 23:56:37 -0400 (Sun,  7 Jun 2020) $';  
our $AUTHOR =	'$Author: Kai Wang <kaichop@gmail.com> $';

# Introduction:
# The coding_change.pl program was originally developed because some users want to have the ability to examine the exact protein sequence or mRNA sequence that a mutation may cause, and compare it against the wildtype sequence themselves. This tool allows users to calculate the mutated sequence and make inference themselves.
# Overtime, additional functionalities were added into the program: the major additional functionalities are (1) some users want to see the pdot annotation for all mutations, and this requires post-processing annotate_variation.pl output. As this functionality is very similar to the original purpose of designing coding_change.pl, I decided to just repurpose coding_change.pl for this purpose. (2) Some users pointed out that annotate_variation.pl has its own set of logic issues (for example, it treats indels as frameshift, yet indels may still cause nonframeshift change on proteins), and they want a more biologically reasonable logic on the protein-level effects. While I incorporated these changes in table_annovar.pl through -polish argument, it is somewhat intuitive that these changes should be done in coding_change.pl, so that table_annovar.pl calls coding_change.pl through -polish argument to address these user concerns. The -newevf argument is introduced at that time, so that if it is specified we need to update the p. notation for the mutation.
# As a result, coding_change.pl has grown into a chimera over time that does multiple things together, and it is more used as a "post-processor" of table-annovar than the original purpose of calculating coding change of DNA level mutations.
# As of 20191010, I decided to just make -polish argument to be ON by default in table_annovar.pl, so that coding_change.pl will be always called by table_annovar.pl for post-processing of gene-based annotations (unless somebody specifies --nopolish)
# Many users have also attempted to modify coding_change.pl to fit their own logic on how to interpret DNA level mutations especially indels and especially indels that covers junctions (such as UTR/exon junction, and UTR/intron junction, and start codon). Therefore there are a few "branched"/"forked" version of coding_change.pl available from other users. Nevertheless, I will just keep a single version of coding_change.pl, based on discussions with several users on what is the most acceptable way to annotate protein-level changes. I do not plan to distribute/keep these "branched" version myself to avoid confusion to most ANNOVAR users.
# The main flow of the program is described below to help users modify the tool to suit your own need, given that too many changes are introduced over the years, and the program is no longer easy to read or understand:
# We require the exonic_variant_function file ($evffile), the gene definition file ($genefile), and the mRNA sequence file for each transcript ($fastafile) as input to the program
# The $evffile is read into the @queue. queue contains all the transcripts and mutations in these transcripts that need to be processed to know protein-level change
# Next we read gene definition file which is in UCSC gene annotation format. The main information is %mrnastart, %mrnaend, so that we know where the coding starts in the mRNA sequence. The name %mrnastart and %mrnaend are actually confusing, and probably should be changed to %%mrnacdsstart, %mrnacdsend instead in the future
# Next we read the FASTA file, and get the actual mRNA sequence for each transcript (some transcripts have errors due to genome assembly error or gene annotation error, and these should be already annotated in the mRNA file that is generated in ANNOVAR)
# Finally, the fun part, we process each element in the queue, based on a preset logic rule. Additional explanations and logic are explained in the code below.
# I hope this Introduction paragraph is informative and helpful to allow users modify the program to suit your own purposes.


# TODO: many users asked to include startgain annotation for UTR5 variants if a new ATG codon is created by the mutation. My thought is that annotate_variation.pl should try to remain stable with minimal change, and these types of changes should be done in coding_change.pl instead via post-processing. This is however more difficult, because the input file need to include the variant_function in addition to exonic_variant_function file from annotate_variation.pl. I need some more time to implement this.

our ($verbose, $help, $man, $includesnp, $mrnaseq, $onlyAltering, $codingseq, $alltranscript, $newevf, $outfile, $tolerate);

our ($evffile, $genefile, $fastafile);
our (%newevf_p, %newevf_function);

our %codon1 = (TTT=>"F", TTC=>"F", TCT=>"S", TCC=>"S", TAT=>"Y", TAC=>"Y", TGT=>"C", TGC=>"C", TTA=>"L", TCA=>"S", TAA=>"*", TGA=>"*", TTG=>"L", TCG=>"S", TAG=>"*", TGG=>"W", CTT=>"L", CTC=>"L", CCT=>"P", CCC=>"P", CAT=>"H", CAC=>"H", CGT=>"R", CGC=>"R", CTA=>"L", CTG=>"L", CCA=>"P", CCG=>"P", CAA=>"Q", CAG=>"Q", CGA=>"R", CGG=>"R", ATT=>"I", ATC=>"I", ACT=>"T", ACC=>"T", AAT=>"N", AAC=>"N", AGT=>"S", AGC=>"S", ATA=>"I", ACA=>"T", AAA=>"K", AGA=>"R", ATG=>"M", ACG=>"T", AAG=>"K", AGG=>"R", GTT=>"V", GTC=>"V", GCT=>"A", GCC=>"A", GAT=>"D", GAC=>"D", GGT=>"G", GGC=>"G", GTA=>"V", GTG=>"V", GCA=>"A", GCG=>"A", GAA=>"E", GAG=>"E", GGA=>"G", GGG=>"G");		#"

# mitochondria codon
our %codon1m = (TTT=>"F", TTC=>"F", TCT=>"S", TCC=>"S", TAT=>"Y", TAC=>"Y", TGT=>"C", TGC=>"C", TTA=>"L", TCA=>"S", TAA=>"*", TGA=>"W", TTG=>"L", TCG=>"S", TAG=>"*", TGG=>"W", CTT=>"L", CTC=>"L", CCT=>"P", CCC=>"P", CAT=>"H", CAC=>"H", CGT=>"R", CGC=>"R", CTA=>"L", CTG=>"L", CCA=>"P", CCG=>"P", CAA=>"Q", CAG=>"Q", CGA=>"R", CGG=>"R", ATT=>"I", ATC=>"I", ACT=>"T", ACC=>"T", AAT=>"N", AAC=>"N", AGT=>"S", AGC=>"S", ATA=>"M", ACA=>"T", AAA=>"K", AGA=>"*", ATG=>"M", ACG=>"T", AAG=>"K", AGG=>"*", GTT=>"V", GTC=>"V", GCT=>"A", GCC=>"A", GAT=>"D", GAC=>"D", GGT=>"G", GGC=>"G", GTA=>"V", GTG=>"V", GCA=>"A", GCG=>"A", GAA=>"E", GAG=>"E", GGA=>"G", GGG=>"G");

GetOptions('verbose|v'=>\$verbose, 'help|h'=>\$help, 'man|m'=>\$man, 'includesnp'=>\$includesnp, 'mrnaseq'=>\$mrnaseq, 'onlyAltering'=>\$onlyAltering, 
	'codingseq'=>\$codingseq, 'alltranscript'=>\$alltranscript, 'newevf=s'=>\$newevf, 'outfile=s'=>\$outfile, 'tolerate'=>\$tolerate) or pod2usage ();
	
$help and pod2usage (-verbose=>1, -exitval=>1, -output=>\*STDOUT);
$man and pod2usage (-verbose=>2, -exitval=>1, -output=>\*STDOUT);
@ARGV or pod2usage (-verbose=>0, -exitval=>1, -output=>\*STDOUT);
@ARGV == 3 or pod2usage ("Syntax error");

($evffile, $genefile, $fastafile) = @ARGV;

$codingseq and $mrnaseq || pod2usage ("Error in argument: --mrnaseq is required when --codingseq is specified");

if ($newevf) {
	defined $alltranscript or pod2usage ("Error in argument: --alltranscript arguments are required when you specify -newevf");
}

if ($outfile) {
	open (STDOUT, ">$outfile") or  die "Error: cannot write to output file $outfile: $!";
}

open (EVF, $evffile) or die "Error: cannot read from evffile $evffile: $!\n";
open (GENE, $genefile) or die "Error: cannot read from genefile $genefile: $!\n";
open (FASTA, $fastafile) or die "Error: cannot read from fastafile $fastafile: $!\n";

#next, we read the exonic_variant_function file
my (@queue, %need_trans);
my (%flagged_transcript);		#transcript with errors (premature stop codon, or no stop codon)
while (<EVF>) {
	s/[\r\n]+$//;
	m/^line\d+/ or die "Error: invalid record found in exonic_variant_function file $evffile (line number expected): <$_>\n";
	my @field = split (/\t/, $_);	#example: line4   nonsynonymous SNV       NOC2L:NM_015658:exon15:c.C1667T:p.S556L,        1       881918  881918  G       A
	$field[2] =~ m/^[\w\-\.\@]+:([\w\.]+):wholegene/ and next;	#do not process mutations affecting whole gene
	$field[2] =~ m/unknown/i and next;				#do not process mutations marked as unknown (due to error in reference gene annotation)
	my $chr = $field[3];		#it is important to know chromosome information here because mitochondria has a different codon table
	
	#ensembl gene name may contain - and . and "TRAV14/DV4" (ENST00000390440), where as UCSC transcript name may contain '.'.
	#knownGene may contain strange characters such as TRA@:uc001wbt.1:exon1:c.C21G:p.S7R
	#sometimes there are negative sign: "SNAPC1:NM_003082:exon1:c.-4_3AGGCGTGC"
	#the following pattern handles all these situations
	$field[2] =~ m/^[\w\-\.\@\/]+?:([\w\.\-]+?):exon\d+:c.([\w\->]+)(:p.([\w\*]+))?/ or die "Error: invalid record found in exonic_variant_function file (exonic format error): <$_>";  #chromsome name can contain dot or - sign
	
	while ($field[2] =~ m/[\w\-\.]+?:([\w\.]+?):exon\d+:c.([\w>]+)(:p.([\w\*]+))?/g) {
		my ($transcript, $cchange, $pchange) = ($1, $2, $4);
		my ($start, $end, $ref, $obs); 
		if ($cchange =~ m/^([ACGTacgt])(\d+)([ACGTacgt])$/) {
			if ($includesnp or $2<=3) {				##first codon (start codon) is changed, so regardless of -includesnp, this is treated as startloss
				($start, $end, $ref, $obs) = ($2, $2, $1, $3);
			} else {
				next;		#by default, we only process indels; so this SNP will be skipped (unless -includesnp is set)
			}
		} elsif ($cchange =~ m/^(\d+)([ACGTacgt])>([ACGTacgt])$/) {	#sometimes c.C1667T is written as c.1667C>T
			if ($includesnp or $1<=3) {
				($start, $end, $ref, $obs) = ($1, $1, $2, $3);
			} else {
				next;
			}
		} elsif ($cchange =~ m/^(\d+)_(\d+)delins(\w+)/) {	#block substitution
			($start, $end, $obs) = ($1, $2, $3);
		} elsif ($cchange =~ m/^(\d+)delins(\w+)/) {		#block substitution for a single nucleotide such as c.3delinsGT
			($start, $end, $ref, $obs) = ($1, $1, 'REF', $2);
		} elsif ($cchange =~ m/^(\d+)del(\w+)/) {		#single base deletion
			($start, $end, $ref, $obs) = ($1, $1, $2, '');
		} elsif ($cchange =~ m/^(\d+)_(\d+)del(\w*)/) {		#multi-base deletion
			($start, $end, $ref, $obs) = ($1, $2, $3, '');
		} elsif ($cchange =~ m/^(\d+)_(\d+)ins(\w+)/) {		#insertion
			($start, $end, $ref, $obs) = ($1, $1, 0, $3);		#if end is equal to start, this is an insertion
		} elsif ($cchange =~ m/^(\d+)dup(\w+)/) {		#insertion
			($start, $end, $ref, $obs) = ($1, $1, 0, $2);
		} elsif ($cchange =~ m/^(\d+)_(\d+)(\w+)/) {		#non-frameshift substitution (example: c.1825_1826TT, now the ref is not included in the string)
			($start, $end, $ref, $obs) = ($1, $2, 'REF', $3);
		} else {
			die "Error: invalid coding change format: <$cchange> within <$_>\n";
		}
		push @queue, [$field[0], $transcript, $start, $end, $ref, $obs, $cchange, $pchange, $chr];
		$need_trans{$transcript}++;	#this transcript is marked since we need it in reading mRNA FASTA files
		
		$alltranscript or last;		#if -alltranscript is set, process all record in this line; otherwise process the first transcript only
	}
}
close (EVF);

#next, we will read the refGene annotation file
my (%mrnastart, %mrnaend);	#key is transcript name, value is the translation start position in this transcript
while (<GENE>) {
	s/[\r\n]+$//;
	my @field = split (/\t/, $_);
	@field >= 11 or die "Error: invalid record found in gene file (>=11 fields expected): <$_>\n";
	$field[0] =~ m/^\d+$/ and shift @field;		#as a general rule, refGene and ensGene has bin as the first column, other gene annotation files do not

	my ($name, $strand, $txstart, $txend, $cdsstart, $cdsend, $exonstart, $exonend) = @field[0, 2, 3, 4, 5, 6, 8, 9];
	$need_trans{$name} or next;		#if this transcript is not needed, we skip it
	
	my ($mrnastart, $mrnaend);		#the start and end site of the translation in the mRNA (if it starts from first exon, it is simply the cdsstart-txstart, but sometimes translation may start from the 2nd or 3rd exon, so we have to be careful here
	
	#next we need to make sure that there is no intron between transcription start and translation start (this is rare but it happens when cdsstart is not in the first exon)
	my @exonstart = split (/,/, $exonstart);
	my @exonend = split (/,/, $exonend);
	
	$txstart++;
	$cdsstart++;
	@exonstart = map {$_+1} @exonstart;	#by default the start is zero-based in gene annotation file, here we change to 1-based to make it easier to process

	if ($strand eq '+') {
		#below illustrate the situation where translation start from the second exon
		#<---->-----<--->----<------>----<----->-----<--->
		#             **********
		my $intron = 0;
		for my $i (0 .. @exonstart-1) {
			$i and $intron += ($exonstart[$i]-$exonend[$i-1]-1);
			if ($cdsstart >= $exonstart[$i] and $cdsstart <= $exonend[$i]) {
				$mrnastart = $cdsstart-$txstart+1-$intron;
			}
			if ($cdsend >= $exonstart[$i] and $cdsend <= $exonend[$i]) {
				$mrnaend = $cdsend-$txstart+1-$intron;
			}
			
		}
	} elsif ($strand eq '-') {
		#<---->-----<--->----<------>----<----->-----<--->
		#             **********
		my $intron = 0;
		for (my $i=@exonstart-1; $i>=0; $i--) {
			$i<@exonstart-1 and $intron += ($exonstart[$i+1]-$exonend[$i]-1);
			if ($cdsend >= $exonstart[$i] and $cdsend <= $exonend[$i]) {
				$mrnastart = $txend-$cdsend+1-$intron;
			}
			if ($cdsstart >= $exonstart[$i] and $cdsstart <= $exonend[$i]) {
				$mrnaend = $txend-$cdsstart+1-$intron;
			}
			
		}
	}

	$mrnastart{$name} = $mrnastart;
	$mrnaend{$name} = $mrnaend;
}
close (GENE);

#next, we read the mRNA FASTA file
my (%mrnaseq);
my ($curname, $curseq, $curstatus);	#current name, current sequence, current status (whether flagged as bad transcript)
while (<FASTA>) {
	s/[\r\n]+$//;
	if (m/^>([\w\.]+)/) {
		if ($curseq) {
			#some transcripts (such as NM_001075) occur multiple times in a file, sometimes with bad ORF annotation
			if ($curstatus) {
				1;	#do not do anything if this is already flagged as a bad transcript
			} else {
				if ($mrnaseq{$curname}) {	#if it already exists, the new one must be longer to replace the old one (it is not really an ideal solution, but we have to make some arbitray decision here when one transcript has multiple sequences that are all valid (non-flagged). Taking the longest one seems a logical approach
					length ($curseq) > length ($mrnaseq{$curname}) and $mrnaseq{$curname} = $curseq;
				} else {
					$mrnaseq{$curname} = $curseq;
				}
			}
		}
		$curname = $1;
		$curseq = '';
		$curstatus = 0;
		if (m/does not have correct ORF annotation/) {	#when retrieve_seq_from_fasta.pl in ANNOVAR generate fasta file from gene annotation file, it will mark "bad" transcripts that do not have correct ORF annotations
			$curstatus = 1;
		}
	} else {
		$curseq .= $_;
	}
}
if ($curseq) {		#process the last sequence in the mRNA FASTA file
	if (not $curstatus) {
		if ($mrnaseq{$curname}) {
			length ($curseq) > length ($mrnaseq{$curname}) and $mrnaseq{$curname} = $curseq;
		} else {
			$mrnaseq{$curname} = $curseq;
		}
	}
}
close (FASTA);

#next, we process each element in the queue
for my $i (0 .. @queue-1) {
	my ($line, $transcript, $start, $end, $ref, $obs, $cchange, $pchange, $chr) = @{$queue[$i]};
	$verbose and print STDERR "NOTICE: Processing $line with transcript=$transcript, start=$start, end=$end, ref=$ref, obs=$obs, cchange=$cchange, pchange=$pchange\n";
	if (not defined $mrnaseq{$transcript}) {
		print STDERR "WARNING: cannot find mRNA sequence for $transcript in the fastafile $fastafile\n";
		next;
	}
	if (not defined $mrnastart{$transcript}) {
		print STDERR "WARNING: cannot find annotation for $transcript in the genefile $genefile or cannot infer the transcription start site\n";
		next;
	}
	if (not defined $mrnaend{$transcript}) {
		print STDERR "WARNING: cannot find annotation for $transcript in the genefile $genefile or cannot infer the transcription end site\n";
		next;
	}
	if ($end > length ($mrnaseq{$transcript})) {
		print STDERR "ERROR: transcript end ($mrnaend{$transcript}) for $transcript is longer than transcript length ${\(length ($mrnaseq{$transcript}))}, skipping this transcript\n";
		next;
	}
	
	
	#when -mrnaseq argument is set, print out mRNA sequences rather than protein sequences
	if ($mrnaseq) {
		my $utr5 = substr ($mrnaseq{$transcript}, 0, $mrnastart{$transcript}-1);
		my $utr3 = substr ($mrnaseq{$transcript}, $mrnaend{$transcript});
		my ($mrna1, $mrna2);
		$mrna1 = $mrnaseq{$transcript};
		
		my $dna = substr ($mrnaseq{$transcript}, $mrnastart{$transcript}-1, $mrnaend{$transcript}-$mrnastart{$transcript}+1);	#this is the coding sequence (though I use $dna to denote it)
		my @dna = split (//, $dna);
		my ($protein1, $protein2);
		my $warning = '';
		
		my $cds = $dna;		#keep a copy of $dna as $cds, because we will change $dna below based on the mutation
	
		if ($end > @dna) {
			print STDERR "ERROR in $line: end position of variant ($end) in $transcript is longer than coding portion length ${\(scalar @dna)}, skipping this transcript\n";
			next;
		}
	
		$protein1 = translateDNA ($dna, $chr);
		$protein1 =~ m/\*\w/ and $warning = '(WARNING: Potential FASTA sequence error!!!)';	#stop codon found inside the translated protein sequence
		
		#the block below calculate the new coding sequence and store in $dna. This calculation depends on whether an insertion is found, or whether any other type of mutations are found
		if ($start == $end and not $ref and $obs) {		#this is an insertion (see the line above, "($start, $end, $ref, $obs) = ($1, $1, 0, $3)")
			splice (@dna, $start, 0, $obs); 
		} else {						#this is a substitution
			splice (@dna, $start-1, $end-$start+1, $obs); 
		}
		$dna = join ('', @dna);		#this is the new coding sequence that contains mutation
		$mrna2 = $utr5 . $dna . $utr3;	#this is the new transcript sequence that contains mutation
		
		if ($codingseq) {	#only print out the coding portion of the sequence
			$mrna1 =~ s/(.{100})/$1\n/g;
			print ">$line $transcript WILDTYPE CODING SEQUENCE $warning\n";
			print "$cds\n";		#$cds is the copy of the original coding sequence
			$mrna2 =~ s/(.{100})/$1\n/g;
			print ">$line $transcript CODING SEQUENCE c.$cchange\n";
			print "$dna\n";		#$dna is the new coding sequence with mutation
		} else {
			$mrna1 =~ s/(.{100})/$1\n/g;
			print ">$line $transcript WILDTYPE $warning\n";
			print "$mrna1\n";	#$mrna1 is the copy of the original transcript sequence
			$mrna2 =~ s/(.{100})/$1\n/g;
			print ">$line $transcript c.$cchange\n";
			print "$mrna2\n";	#$mrna2 is the new transcript sequence with mutation
		}


	#process protein sequence change (when -mrnaseq argument is not set)
	} else {
		#dna is the coding DNA sequence (from first to last codon)
		my $dna = substr ($mrnaseq{$transcript}, $mrnastart{$transcript}-1, $mrnaend{$transcript}-$mrnastart{$transcript}+1);
		
		#it is a little confusing below since we used both $dna and @dna yet they are different (@dna contains UTR3)
		#@dna is the mRNA sequence (from first codon to the end of the mRNA including UTR, because we want to be able to handle situations where UTR is included in protein calculation)
		my @dna = split (//, substr ($mrnaseq{$transcript}, $mrnastart{$transcript}-1));  #now DNA includes the UTR3 region (to handle stop loss scenario, since stoploss will result in longer transcript)
		my ($protein1, $protein2);
		my $warning = '';
		my $inframe;
	
		#$verbose and print STDERR "NOTICE: end=$end length=", length($dna), "\n";
		$verbose and print STDERR "NOTICE: wild-type DNA coding sequence is $dna\n";
		
		$protein1 = translateDNA ($dna, $chr);	#protein1 is the wildtype protein sequence
		my $protein1_orig = $protein1;		#the original sequence without deleting the part after stop codon
		$verbose and print STDERR "NOTICE: wild-type protein sequence is $protein1\n";
		
		#20170712: I decided to impose a stringent criteria here to make sure that amino acid change calculation is correct
		#if ($protein1 =~ m/\*.+/ or $protein1 !~ m/\*$/) {
		if ($protein1 =~ m/\*.+/) {
			$flagged_transcript{$transcript}++;
			$warning = '(WARNING: Potential FASTA sequence error!!!)';
			if ($tolerate) {	#tolerate sequence error
				$protein1 =~ s/\*.+/\*/;	#when -tolerate is set, we delete everything after stop codon, but still process this transcript/protein
			} else {
				next;
			}
		}
		
		#the following block essentially generates the mutated mRNA sequence, by replacing ref allele by alt allel, and give preliminary assessment on inframe status
		if ($start == $end and not $ref and $obs) {		#this is an insertion (ref is treated as 0 when we read the EVF file previously)(see the line above, "($start, $end, $ref, $obs) = ($1, $1, 0, $3)")
			splice (@dna, $start, 0, $obs);
			$start++;
			$end++;		#after insertion of $obs, the position of start/end increase by one
			if (length($obs) % 3 == 0) {
				$inframe++;
			}
		} else {				#this is a substitution
			if ($obs eq '') {		#deletion
				splice (@dna, $start-1, $end-$start+1);
				if (($end-$start+1) % 3 == 0) {
					$inframe++;
				}
			} else {
				splice (@dna, $start-1, $end-$start+1, split(//, $obs));	#20170712: absolutely important here to split obs into individual elements in an array!!!
				if (($end-$start+1-length($obs)) % 3 == 0) {
					$inframe++;
				}
			}
		}
		
		my $aastart = int(($start-1)/3)+1;			#start position in wildtype or mutated protein, assuming that start position is always the same (the only exception would be a ATG start codon change, which we address below)
		my $aaend1 = int (($end-1)/3)+1;			#end position in wild type protein
		my $aaend2 = int (($start+length($obs)-1-1)/3)+1;	#end position in mutated protein
		my ($function, $aachange);
		$aachange = '';		#assign default value, since synonymous variants do not have aachange
		
		$dna = join ('', @dna);			#the new @dna array contains mutations that are inserted or substituted
		
		if ($aastart == 1) {
			$protein2 = translateDNA ($dna, $chr, 'wiggle');	#if mutation affects the first amino acid, we allow wiggle room
		} else {
			$protein2 = translateDNA ($dna, $chr);	#protein2 is the mutated protein sequence (possibly including UTR3's translation, if stoploss mutation is found)
		}
		
		$verbose and print STDERR "NOTICE:   mutated DNA coding+UTR3 sequence is $dna\n";
		$verbose and print STDERR "NOTICE:   mutated protein sequence (with stop codon and AA after stop codon) is $protein2\n";
				
		#20170927: protein2 should be truncated so that anything after stop codon be deleted
		$protein2 =~ s/\*.+/\*/;
				
		$verbose and print STDERR "\n\nNOTICE: protein1=$protein1\nNOTICE: protein2=$protein2\n";
		
		if (length ($protein2) > $aastart-1 and substr ($protein2, $aastart-1, $aaend2-$aastart+1) =~ m/\*/) {
			$function = 'immediate-stopgain';
		} elsif ($end>=$mrnaend{$transcript}-2) {
			$function = 'immediate-stoploss';		#the mutation covers the last codon (-3, -2, -1 position of the stop codon)
		} else {
			if ($protein1 eq $protein2) {
				$function = 'silent';
				$onlyAltering and next;
				#print STDERR "NOTICE function=$function\n";
			} else {					#protein1 and protein2 have different lengths
				if ($aastart == 1) {
					if (substr($dna, 0, 3) eq 'ATG') {		#the $dna is not the second DNA sequence after mutation (20180928)
						$function = 'protein-altering';	#this is not startloss because the start codon is still ATG
					} elsif ($protein1 =~ m/^M/ and not $protein2 =~ m/^M/) {	#extra AA before start codon
						#here we need to handle the special situation that the start codon (M) is changed, but the protein translation can still start from a different start codon
						#since the translation only starts from ATG, we have to make sure that we examine both AA level and DNA level changes
						my $first_codon_newaa = substr($protein2, 0, $aaend2);
						$protein2 =~ s/^[^M]+M/M/;		#trim off the AA before M
						$verbose and print STDERR "\n\nNOTICE AGAIN: protein1=$protein1\nNOTICE AGAIN: protein2=$protein2\n";
						if ($protein1 eq $protein2) {
							$function = 'silent';
						} elsif ($first_codon_newaa =~ m/M/) {	#M is still present in the first codon
							$function = 'protein-altering';
						} else {
							$function = 'startloss';
						}
					} else {
						#a special scenario that we need to consider is something like "10:74928098-74928098 - GGA", original start ATG becomes AGGATG, so there is still a start codon ATG
						$function = 'startloss';	#201807, a special case scenario, when the first amino acid is changed, it is treated as startloss
					}
				} else {
					$function = 'protein-altering';		#change nonsynonymous to protein-altering to reduce confusion among users 20150604
				}
			}
		}
		
		#I changed the paragraph below 20150604 to reduce confusion among users: for insertions, I now identify the exact position that has change (rather thant the position where nucleotide changes)
		#$aachange = " (position $aastart-$aaend1 changed from " . substr ($protein1, $aastart-1, $aaend1-$aastart+1) . ' to ' . substr ($protein2, $aastart-1, $aaend2-$aastart+1). ')';
		#for my $i (0 .. length($protein1)-1) {
		#	if (substr($protein1, $i, 1) ne substr($protein2, $i, 1)) {
		#		$aachange = " (position ${\($i+1)} changed from " . substr($protein1, $i, 1) . ' to ' . substr($protein2, $i, 1) . ")";
		#		last;
		#	}
		#}
		
		#Per user requests, I further changed the above paragraph to the updated one below.
		#The goal is to find the exact change of all amino acids (not just the first one), until a stop codon is found
		my ($pos1, $pos2, $aa1, $aa2);		#pos1: first position with difference; aa1:peptide with difference
		my $len_max = length($protein1);
		$len_max < length($protein2) and $len_max = length($protein2);
		my @protein1 = split (//, $protein1);
		my @protein2 = split (//, $protein2);
		#print STDERR "line 287 Process next variant ($line, $transcript, $start, $end, $ref, $obs, $cchange)\nprotein1=$protein1\nprotein2=$protein2\n";
		for my $i (0 .. $len_max-1) {
			if ($pos1) {	#already in difference zone 
				#$i < length ($protein1) or die "Error: ($line, $transcript, $start, $end, $ref, $obs, $cchange)\n$protein1\n$protein2";
				#$i < length ($protein1) and $aa1 .= substr($protein1, $i, 1);
				#$i < length ($protein2) and $aa2 .= substr($protein2, $i, 1);
				$i < length ($protein1) and $aa1 .= $protein1[$i];
				$i < length ($protein2) and $aa2 .= $protein2[$i];
			} else {
				#if (substr($protein1, $i, 1) ne substr($protein2, $i, 1)) {
				#	$pos1 = $i+1;
				#	$aa1 = substr($protein1, $i, 1);
				#	$aa2 = substr($protein2, $i, 1);
				#}
								
				$i > length($protein1) and next;
				$i > length($protein2) and next;
				if ($i == length($protein1)) {		#sometimes a mutation cause stop loss so the protein2 is much longer (with UTR sequence); however, clearly the transcript has problems since * is not found
					$pos1 = $i+1;
					$aa1='';
					$aa2=$protein2[$i];
				} elsif ($i == length($protein2)) {	#protein2 is shorter and now it reached
					$pos1 = $i+1;
					$aa1=$protein1[$i];
					$aa2='';
				} elsif ($protein1[$i] ne $protein2[$i]) {
					$pos1 = $i+1;
					$aa1 = $protein1[$i];
					$aa2 = $protein2[$i];
				}
			}
			
			#20170927: I commented out the entire section below. (protein2 is now processed so that amino acids after * are deleted above)
			#if ($i >= length($protein2)) {
			#	next;		#do not consider protein2 now, but we still need to get all the aa1
			#}
			#if ($protein2[$i] eq '*') {	#stop codon found in mutated protein
			#	if ($i < length ($protein1)) {
			#		$pos2 = $i+1;
			#	} else {
			#		$pos2 = length ($protein1);
			#	}				
			#	last;
			#}
		}
		
		#special case: in hg19, chr6:30558477->A is a silent mutation on the stop codon, but previous version of the tool cannot recognize it correctly.
		if (not $pos1) {	#unable to find the first position with difference with mutated protein, so this is not a protein-altering mutation
			#next;		#process next mutation in the queue instead (I think even if this is not protein-altering, we should still print it out as exonic_variant_function did not think so)
		}
		
		#maximum of pos2 is the end of the wildtype protein
		if (not $pos2) {
			$pos2 = length($protein1);
		}

		#sometimes there is a nonframeshift mutation, and I previously calculate the protein until stop codon, which is inconvenient.
		#the following code addresses this problem.
		#$verbose and print STDERR "Current pos1=$pos1 pos2=$pos2 aa1=$aa1 aa2=$aa2\n";
		#additional update 20191010: there are scenarios where a frameshift mutation creates the same tail as the wildtype sequence, for example "KRR*" for 37_15-89803991-TA-T. Do we treat this as a nonframeshift deletion, or as a frameshift premature stop codon? This is difficult to gauge, and probably has to be based on the actual length of amino acid track that is deleted.
		#additional update 20191010: due to the above reason, I now added "if($infame)" below
		if ($inframe) {
			while (1) {		#sometimes aa1 and aa2 may have identical tails
				if ($pos1 and $aa1 ne '*' and substr($aa1, -1) eq substr($aa2, -1)) {
					$aa1 =~ s/.$//;
					$aa2 =~ s/.$//;
					$pos2--;
				} else {
					last;
				}
			}
		}
		#$verbose and print STDERR "After tail trimming pos1=$pos1 pos2=$pos2 aa1=$aa1 aa2=$aa2\n";

		#print STDERR "NOTICE 453 transcript=$transcript function=$function pos1=$pos1 pos2=$pos2 aa1=$aa1 aa2=$aa2\n";
		#The following black tries to re-assign functional consequence to indels
		#pos1 and pos2 are positions in wildtype protein, where differences occur
		#When $pos1 is undefined, it means there is no difference between the original protein and the mutated protein
		#when $pos1 is equal to $pos2, it is either non-synonymous, or inframe insertion
		#when $pos1 is larger than $pos2, it is insertion
		#otherwise, it is just a standard deletion or block substitution
		
		#print STDERR "------------------------pos1=$pos1 pos2=$pos2 cchange=$cchange pchange=$pchange function=$function---------------------------------\n";
		
		if (not $pos1) {
			$aachange = " (no amino acid change)";
			if (not defined $pchange) {
				#it is actually not that simple to handle this situation of silent change. We have to calculate it from c change instead
				if ($cchange =~ m/(\d+)/) {
					my $pchange_pos = int(($1-1)/3)+1;			#20180626: add "+1" since the starting position of c. notation is 1, and this should translate to p. notation of 1 as well
					
					if ($pchange_pos > length($protein1)) {	#something is wrong here, the wildtype sequence is not complete possibly (I changed >= to > on 20180630)
						$flagged_transcript{$transcript}++;
						next;
					}
					my $pchange_aa = substr($protein1, $pchange_pos-1, 1);
					$pchange = "$pchange_aa$pchange_pos$pchange_aa";
				}
			} elsif ($pchange =~ m/([\w\*]+?)(\d+)fs/) {
				$pchange = "$1$2$1";
			} elsif ($pchange =~ m/(\d+)_(\d+)del/) {	#an example is "17      44060738        44060740        CAC     -", where there is annotation error in the gene definition
				#$aa1 = substr($protein1_orig, $1-1, 1);
				#defined $aa1 or $aa1 = '*';
				$pchange = "*$1*";		#use *pos* to denote potential errors in the annotation
			} elsif ($pchange =~ m/M1delins/) {	#20181215: first codon change with new ATG inside (so protein does not really change)
				$pchange = "M1M";
			}
			
			if ($function ne 'startloss') {
				$function = 'silent';
			}
		} elsif ($pos1 == $pos2) {		#a simple non-synonymous change, or an inframe insertion if length of aa1!=aa2
			$aachange = " (position $pos1 changed from $aa1 to $aa2)";
			$pchange = $aa1 . $pos1 . $aa2;
			if ($aa1 eq '*') {		
				$function = 'immediate-stoploss';
			}
			if ($aa2 eq '*') {		
				$function = 'immediate-stopgain';
			}
			if ($aa1 eq $aa2) {
				$function = 'silent';	#aa1 and aa2 could both be * (stop codon)
			}
			
			if ($aa2 eq '') {
				$pchange .= 'del';
			} elsif (length($aa1) != length($aa2)) {	#inframe insertion such as p.V253delinsEVL for 35874601	35874601	-	AGGTCC
				$pchange = $aa1 . $pos1 . 'delins' . $aa2;
			}
			
		} elsif ($pos1 > $pos2) {	#when there is an insertion, pos2 will be 1 less than pos1 (example: 1	1647893 1647893 -       TTTCTT)
			$aachange = " (position $pos2-$pos1 has insertion $aa2)";
			$pchange = substr($protein1, $pos2-1, 1) . $pos2 . '_' . substr($protein1, $pos1-1, 1) . $pos1 . 'ins'. $aa2;
		} else {
			
			if ($function eq 'startloss') {
				$pchange = 'M1?';		#earlier on we already calculated this as startloss (it could happen for something like p.A2_M145del, so pos1!=1)
			} else {
				if ($inframe) {		#inframe substitution
					$aachange = " (position $pos1-$pos2 changed from $aa1 to $aa2)";
					$pchange = substr($aa1, 0, 1) . $pos1. '_' . substr($aa1, length($aa1)-1, 1) . $pos2 . 'delins' . $aa2;
					$aa2 eq '' and $pchange =~ s/ins$//;	#when aa2 is empty, it should be just deletion, not delins
				} else {
					$aachange = " (position $pos1-$pos2 changed from $aa1 to $aa2)";
					
					if ($aa2 eq '*') {
						if ($pos1 == 1) {
							$function = 'startloss';
						} else {
							$function = 'immediate-stopgain';
						}
						$pchange = substr($aa1, 0, 1) . $pos1. substr($aa2, 0, 1);	
					} else {
						if ($pos1 == 1) {
							$function = 'startloss';		#whenever M1 is affected, we will just call it startloss and give a p.M1? notation
							$pchange = substr($aa1, 0, 1) . $pos1. '?';
						} else {
							$pchange = substr($aa1, 0, 1) . $pos1. substr($aa2, 0, 1) . 'fs*' . length($aa2);
						}
					}
				}
			}
		}
		#print STDERR "NOTICE 513 function=$function pos1=$pos1 pos2=$pos2\n";		
		$protein2 =~ s/\*.+/\*/;		#delete anything after the STOP codon
	
		$protein1 =~ s/(.{100})/$1\n/g;
		print ">$line $transcript WILDTYPE $warning\n";
		print "$protein1\n";
		$protein2 =~ s/(.{100})/$1\n/g;
		print ">$line $transcript c.$cchange p.$pchange $function $aachange\n";
		print "$protein2\n";
		
		if ($newevf) {		#when -newevf is specified we need to update the p. notation for the mutation, and sometimes even the exonic-variant-function
			$newevf_p{$line, $transcript} = $pchange;
			$newevf_function{$line, $transcript} = $function;
		}
	}
}


#process the new EVF file
if ($newevf) {
	open (NEWEVF, ">$newevf") or die "Error: cannot write to new EVF file: $!";
	open (EVF, $evffile) or die "Error: cannot read from evffile $evffile: $!\n";	#open EVF again since we will work from the begainning
	
	while (<EVF>) {
		#example: line78  nonsynonymous SNV       TNFRSF14:NM_001297605:exon1:c.A50G:p.K17R,TNFRSF14:NM_003820:exon1:c.A50G:p.K17R,       1       2488153 2488153 A       G
		s/[\r\n]+$//;
		m/^line\d+/ or die "Error: invalid record found in exonic_variant_function file $evffile (line number expected): <$_>\n";
		my @field = split (/\t/, $_);
		#if ($field[2] =~ m/^\w+:(\w+):wholegene/ or $field[2] =~ m/unknown/i) {		#the problem is that sometimes the 2nd or 3rd entry is whole gene, so I commented this line 20170712 and handle this situation later
		#	print NEWEVF $_, "\n";
		#	next;
		#}
		
		$field[2] =~ s/,$//;	#delete the last comma
		my @notation = split (/,/, $field[2]);
		my $new_notation;
		for my $i (0 .. @notation-1) {
			if ($notation[$i] =~ m/^[\w\-\.\@]+:([\w+\.]+):wholegene/ or $field[2] =~ m/unknown/i) {
				$new_notation .= $notation[$i] . ',';
				next;
			}
			
			$notation[$i] =~ m/^([\w\-\.\@\/]+?):([\w\.]+?):(exon\d+):(c.[\w\->]+)/ or die "Error: invalid notation found: <$notation[$i]>";
			my ($genename, $transcript, $exon, $cdot) = ($1, $2, $3, $4);
			#$flagged_transcript{$transcript} and next;		#this transcript is flagged for potential error so we simply skip it
			if ($newevf_function{$field[0], $transcript}) {
				#two special situation to handle: (1) silent mutation that was previously treated as frameshift (2) stopgain mutation
				if ($newevf_function{$field[0], $transcript} eq 'silent') {	#switch nonframeshift to frameshift, example is 8       8887543 8887545 AAC     -
					if ($field[1] =~ m/^frameshift/) {
						$field[1] = 'non'.$field[1];
					}
					
					#if ($field[1] =~ m/^nonframeshift/) {		#20181215: the situation with FAM149B1:NM_173348:exon1:c.1_2insGGA:p.M1delinsRM as an example, to handle first codon
					#	$field[1] = 'synonymous SNV';
					#}
				}
				
				if ($newevf_function{$field[0], $transcript} eq 'immediate-stoploss' and $field[1] ne 'startloss' and $field[1] ne 'stopgain') {		#startloss has a higher precedence than stoploss
					$field[1] = 'stoploss';
				}
				if ($newevf_function{$field[0], $transcript} eq 'immediate-stopgain' and $field[1] ne 'startloss') {		#startloss has a higher precedence than stopgain
					$field[1] = 'stopgain';
				}
				if ($newevf_function{$field[0], $transcript} eq 'startloss') {	
					$field[1] = 'startloss';
				}
				$new_notation .= join (":", $genename, $transcript, $exon, $cdot, 'p.'.$newevf_p{$field[0], $transcript}) . ',';
			} else {
				$new_notation .= $notation[$i] . ',';
			}
		}
		
		$field[2] = $new_notation;	#replace old by new
		print NEWEVF join("\t", @field), "\n";
	}
	%flagged_transcript and print STDERR "Warning: ${\(scalar (keys %flagged_transcript))} transcripts are flagged as having potential ORF issues (premature stopcodon or lack of stop codon)\n";
}




#translate a stretch of DNA sequence into a protein sequence
sub translateDNA {
	my ($seq, $chr, $wiggle) = @_;
	my $nt3;
	my $protein = '';
	$seq = uc $seq;
	#length ($seq) % 3 == 0 or printerr "WARNING: length of DNA sequence to be translated is not multiples of 3: <length=${\(length $seq)}>\n";
	#20191010: the only complication here is to handle start codon (ATG) differently. we want to ensure that the translation starts from ATG when feasible, but in practice some reference transcripts do not start with ATG
	#20191010: as a compromise, currently we handle the situation using a somewhat arbitrary rule: if one or two nucleotide can be deleted to create a ATG start codon, we will do it (using the code below)
	
	#Mitochondrial genomes use alternate start codons more significantly (AUA and AUU in humans), so we do not use wiggle for mitochondria
	if (uc $chr ne 'M' and uc $chr ne 'MT' and $wiggle) {	
		$seq =~ s/^..ATG/ATG/;	#delete the first two bases if this creates a ATG start codon
		$seq =~ s/^.ATG/ATG/;	#delete the first base if this creates a ATG start codon
	}
	
	while ($seq =~ m/(...)/g) {
		if (uc $chr eq 'M' or uc $chr eq 'MT') {		#process chrM variants correctly
			defined $codon1m{$1} or print "WARNING: invalid triplets found in DNA sequence to be translated: <$1> in <$seq>\n" and die;
			$protein .= $codon1m{$1};
		} else {
			defined $codon1{$1} or print "WARNING: invalid triplets found in DNA sequence to be translated: <$1> in <$seq>\n" and die;
			$protein .= $codon1{$1};
		}
	}
	return $protein;
}



=head1 SYNOPSIS

 coding_change.pl [arguments] <exonic-variant-function-file> <gene-def-file> <fasta-file>

 Optional arguments:
        -h, --help                      print help message
        -m, --man                       print complete documentation
        -v, --verbose                   use verbose output
            --includesnp		process SNPs (default is to process indels only)
            --mrnaseq			print out mRNA sequence (including UTR and coding)
            --onlyAltering		ignore synonymous SNPs (when -includesnp is on)
            --codingseq			only print coding sequence without UTR (when -mrnaseq is specified)
            --alltranscript		process all transcript rather than the first one (default)
            --newevf <file>		generate a new exonic-variant-function file with updated c. and p.
            --tolerate			tolerate premature stop codon due to annotation error in gene definition file

 Function: infer the translated protein sequence (or mRNA sequence) for exonic 
 frameshift mutations (or SNPs) identified by ANNOVAR
 
 Example: coding_change.pl ex1.avinput.exonic_variant_function humandb/hg19_refGene.txt humandb/hg19_refGeneMrna.fa
          coding_change.pl ex1.avinput.exonic_variant_function humandb/hg19_refGene.txt humandb/hg19_refGeneMrna.fa -includesnp -onlyAltering
 
 Version: $Date: 2020-06-07 23:56:37 -0400 (Sun,  7 Jun 2020) $

=head1 OPTIONS

=over 8

=item B<--help>

print a brief usage message and detailed explanation of options.

=item B<--man>

print the complete manual of the program.

=item B<--verbose>

use verbose output.

=item B<--outfile>

specify the output file name (by default output is written to STDOUT)

=item B<--includesnp>

include sequences affected by SNPs in the output. By default, only sequences 
affected by exonic indels are printed out.

=item B<--mrnaseq>

print out mRNA sequences rather than protein sequences.

=item B<--onlyAltering>

when -includesnp is set, do not process synonymous variants and only print out
protein-coding alterations

=item B<--codingseq>

when -mrnaseq is specified, only print coding sequence without UTR sequence

=item B<--alltranscript>

process all transcripts for a mutation and print out separate coding changes. by
default only  the first  transcript in  the line  in the exonic_variant_function
file is processed.

=item B<--newevf>

generate  an  updated  exonic-variant-function  file  with  updated  c.  and  p.
notation,  based  on  newly  calculated  protein  sequence.  The  -alltranscript
arguments are required for this argument.

=item B<--tolerate>

tolerate premature stop codon due to annotation error in gene definition file

=back

=head1 DESCRIPTION

This program will infer the protein sequence for frameshift mutations identified 
by ANNOVAR. Typically, for non-synonymous mutations, the annotate_variation.pl 
program in ANNOVAR will report the amino acid change at the position; however, 
for frameshift mutations which may affect a long stretch of amino acid, ANNOVAR 
will only give a frameshift annotation without printing out the new protein 
sequence. This program can take ANNOVAR exonic_variant_function file and attempt 
to infer the new protein sequence.

The <exonic-variant-function-file> is the one generated by
annotate_variation.pl. The <gene-def-file> is the gene definition file that is
used by annotate_variation.pl (which can be examined by looking at the LOG
message. The <fasta-file> is also used by annotate_variation.pl (which can be
examined by looking at the LOG message

=over 8

=item * B<Known Bug>

This program does not handle mitochondria mutations correctly yet.

=back

For questions, comments or bug reports, please contact me at 
$Author: Kai Wang <kaichop@gmail.com> $.

=cut
