#include<iostream>
#include<fstream>
#include<string>
#include<sstream>
#include<array>

template<int N,char delim>
inline constexpr std::array<std::string,N> split(const std::string&& sin){
	std::array<std::string,N> ar;
	std::stringstream ss(sin);
	std::string s;

	for(int i =0;i<N;i++){
		std::getline(ss,s,delim);
		ar[i]=s;
	}
	return ar;
}

//inline std::string printDateTime(const std::string& Date,const std::string& Time){
#define printDateTime(Date,Time) \
	 Date.substr(0,2)<<'-'\
	 <<Date.substr(2,2)<<'-'\
	 <<Date.substr(4,2)<<'T'\
	 <<Time.substr(0,2)<<':'\
	 <<Time.substr(2,2)<<':'\
	 <<Time.substr(4,2)
	



int main(int argc,char *argv[]){
	constexpr int cols=20;
	std::ifstream f(argv[1]);
	std::string line="";
	std::array<std::string,cols> ar;

	std::cout<<"PtID_st PtID_end ICAO_orig ICAO_dest AC_type Date_st Date_end Callsign Lat_st Lon_st Lat_end Lon_end FlID seq Length"<<std::endl;
	while(std::getline(f,line,'\n')){
		
		ar=std::move(split<cols,' '>(std::move(line)));

		int und=0;
		und=ar[0].find('_',0);
		std::cout<<ar[0].substr(0,und)//PtID_st
		    <<' '<<ar[0].substr(und+1);//PtID_end

		std::cout<<' '<<ar[1]//ICAO_orig
		         <<' '<<ar[2]//ICAO_dest
		         <<' '<<ar[3];//AC_type
		std::cout<<' '<<printDateTime(ar[10],ar[4])
		         <<' '<<printDateTime(ar[11],ar[5]);

		std::cout//<<' '<<ar[6]//FL_st
		         //<<' '<<ar[7]//FL end
	//	         <<' '<<ar[8]//FL status
		         <<' '<<ar[9];//Callsign


		for(int i=12;i<12+4;i++){//Lats and longs
			std::cout<<' '<<ar[i];
		}

		std::cout<<' '<<ar[16]//FlID
		         <<' '<<ar[17]//seq
		         <<' '<<ar[18];//Length

		//std::cout<<ar[19];//Color

		std::cout<<'\n';
	}
	std::cout.flush();



}
