import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemListener;
import java.text.NumberFormat;

import javax.swing.JButton;
import javax.swing.JTextField;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.ResultSet;
import java.sql.SQLException;

import  javax.swing.*;
import  javax.swing.border.*;
import  java.awt.*;
import javax.swing.table.*; 
import java.text.*;
import java.io.*;


public class MainFrame extends javax.swing.JFrame {
	 	
    private JFileChooser fileChooser= new JFileChooser();  
    private JCheckBox eqel1;
    private JCheckBox eqel2;
    private JComboBox namesComboBox3;
    private Graphics g;
    private JLabel label_EQ1;
    private JLabel label_EQ2;
    private JLabel label_ildc;	
    private JLabel label_igc;	
    private JLabel label_tldc;	
    private JLabel label_tgc;
    private JLabel label_scheme2;	
    private JLabel label_isf;	
    private JLabel jLabelrcZ;
    private JLabel jLabeldimcutZ;
    private JLabel label_chvi;
    private JLabel label_periode;		
    private JCheckBox chut1,chut2;
    private JFrame cadre;
    private JTextArea tA;
    private File fichierCourant = null;
    private JButton bdata;
    private JButton brun;
    private JButton bgenejdd;
    private JButton bresume;	
    private JTextField NB_NODES_X;
    private JTextField NB_NODES_Y;
    private JTextField NB_NODES_Z;
    private JTextField PAS_TEMPS;
    private JTextField CFL;
    private JTextField T_MAX;
    private JTextField I_MAX;
    private JTextField L_MAX;
    private JTextField ISF_MAX;
    private JLabel label_IF;
    private JTextField MIN_X;	
    private JTextField MIN_Y;	
    private JTextField MIN_Z;	
    private JTextField MAX_X;	
    private JTextField MAX_Y;	
    private JTextField MAX_Z;
    private JTextField RC_X;	
    private JTextField RC_Y;	
    private JTextField RC_Z;	
    private JTextField DIMCUT_X;	
    private JTextField DIMCUT_Y;	
    private JTextField DIMCUT_Z;	
    private JTextField ILDC_MAX;	
    private JTextField IGC_MAX;
    private JTextField TLDC_MAX;	
    private JTextField TGC_MAX;
    private JTextField TOL_SEN;	
    private JTextField TOL_GRCL;
    private JTextField FREQ_FRACVOL;	
    private JTextField FREQ_VITESSE;	
    private JTextField F_AMR;	
    private JTextField MAJ_PERIODE;
    private JComboBox namesComboBox4;		
    private JComboBox namesComboBox5;
    private JComboBox namesComboBox21;
    private JComboBox namesComboBox14;
    private JComboBox namesComboBox144;	
    private JButton bdiscre;
    private JButton bposttrait;
    private JButton bamr;
    private JButton bresol;
    private JButton bmeth;
    public Integer NX=0,NY=0,NZ=0,TC=0;
    public Integer ind_cbut=0,ind_ciut= 0;
    public Double TMAX=0.0;
    public Integer IMAX=0;	
    public Double MINX=0.0,MINY=0.0,MINZ=0.0;
    public Double MAXX=0.0,MAXY=0.0,MAXZ=0.0;
    public Double DT=0.0,CFLD=0.0,MAJPERIODE=0.0;
    public Integer ILDCMAX=0;				
    public Integer IGCMAX=0;				
    public Double TLDCMAX=0.0;				
    public Double TGCMAX=0.0;
    public Integer EQ2_YES=0;				
    public Integer LMAX=1;				
    public Integer ISFMAX=0;				
    public Integer RCX=0,RCY=0,RCZ=0;
    public Integer DIMCUTX=0,DIMCUTY=0,DIMCUTZ=0;
    public Integer NB_NODES_Z_YES=2;
    public Integer RC_Z_YES=2;
    public Integer DIMCUT_Z_YES=2;
    public Double TOLSEN=0.1;
    public Double TOLGRCL=0.8;
    public Integer FAMR=1;
    public Integer FREQFRACVOL=1;
    public Integer FREQVITESSE=1;
    public Integer OK1=0,OK2=0,OK3=0,OK4=0;
    public Integer OK5=0,OK6=0,OK7=0;
    public String TC1="CHOISISSEZ";
    public String TC2="SEQUENTIELLE";
    public String TC3="PARALLELE";
    public String CL1="CHOISISSEZ";
    public String CL2="PERIODIQUE";
    public String CI1 = "CHOISISSEZ";
    public String CI2 = "BULLE CARREE";
    public String CI3 = "BULLE ZALESAK";
    public String CI4 = "BULLE CERCLE_KOTHE";
    public String CI5 = "BULLE SPHERE_KOTHE";
    public String CI6 = "BULLE CUBE";
    public String SC1 = "CHOISISSEZ";
    public String SC2 = "SCHEMA LAGOUTIERE";
    public String SC3 = "SCHEMA UPWIND";
    public String DIR1 = "TYPE DE DIRECTION";
    public String DIR2 = "DIRECTION_ALTERNEE";
    public String DIR3 = "DIRECTION_NON_ALTERNEE";
    public String CH1 = "CHOISISSEZ";
    public String CH2 = "DIRECTION_X";
    public String CH3 = "DIRECTION_Y";
    public String CH4 = "DIRECTION_Z";
    public String CH5 = "DIRECTION_DIAG";
    public String CH6 = "DIRECTION_KOTHE_RIDER";
    public String ME2 = "LDC";
    public String ME1 = "CHOISISSEZ";
    public String CH14 = "CHOISISSEZ";
    public String CH24 = "DIRECTION_DILATATION_POS";
    public String CH34 = "DIRECTION_DILATATION_NEG";
    public String CH44 = "DIRECTION_DILATATION_OSCILLATIONS";
    public String CH54 = "DIRECTION_RESONANCE";
    public String CH64 = "DIRECTION_DILATATION_POS_NEG";

    public Integer TEST1=0,TEST2=0,TEST3=0,TEST4=0;
    public Integer TEST5=0,TEST6=0,TEST7=0,TEST8=0;
    public Integer TEST9=0,TEST10=0,TEST11=0,TEST12=0;
    public Integer TEST13=0,TEST14=0,TEST15=0,TEST16=0;
    public Integer TEST17=0,TEST18=0,TEST19=0,TEST20=0;
    public Integer TEST21=0,TEST22=0,TEST23=0,TEST24=0;
    public Integer TEST25=0,TEST26=0,TEST27=0,TEST28=0;
    public Integer TEST29=0,TEST30=0, TEST31=0;	

    public  static  void    main(String args[])
    {
        MainFrame    f=new MainFrame();
	f.setVisible(true);
    }
	 
	public MainFrame(){
        //Définit un titre pour votre fenêtre
        this.setTitle("Interface Graphique du Code TritonAMR");
        //Définit une taille pour celle-ci ; ici, 510 px de large et 200 px de haut
        this.setSize(1000, 700);
        //Nous allons maintenant dire à notre objet de se positionner au centre
        this.setLocationRelativeTo(null);
        //Ferme-toi lorsqu'on clique sur "Fermer" !
        this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        //on creer un container:
        JPanel top = new JPanel();
	top.setLayout(null);


/**
	PARTIE DE DEFINITION DU NOM DU CODE ET DES PROPREITAIRE
**/

 	JLabel label_code = new JLabel("Jeu De Donnees - Code TritonAMR\n");
 	label_code.setBounds(450,20,500,25);
	top.add(label_code);
 	JLabel label_prop = new JLabel("CEA - ONERA\n");
 	label_prop.setBounds(500,50,500,25);
	top.add(label_prop);

	// UN SEPARATEUR
	JSeparator js = new JSeparator();
	js.setBounds(0,70,1000,50);
	top.add(js);

/**
*/
	ImageIcon im1 = new ImageIcon("IMAGE1.png");
	JLabel toto1 = new JLabel(im1);
 	toto1.setBounds(600,75,300,300);
	top.add(toto1);
	ImageIcon im2 = new ImageIcon("IMAGE2.png");
	JLabel toto2 = new JLabel(im2);
 	toto2.setBounds(100,75,300,300);
	top.add(toto2);

/**
	PARTIE DE CONSTRUCTION DES MENUS
**/
	MenuBar mbar = new MenuBar(); 
	Menu menuPrinc1 = new Menu("Fichier");  
	Menu menuPrinc2 = new Menu("Outils"); 
	Menu menuPrinc3 = new Menu("Aide");  
	MenuItem iouvrir = new MenuItem("Ouvrir"); 

 	iouvrir.addActionListener(new ActionListener(){
 	public void actionPerformed(ActionEvent e){
 		ouvrirProjet();
 			}	
 		});
	MenuItem ienregistrer = new MenuItem("Enregistrer");
	ienregistrer.addActionListener(new ActionListener(){
		public void actionPerformed(ActionEvent e){
				}	
			});
	MenuItem iquitter = new MenuItem("Quitter");
	iquitter.addActionListener(new ActionListener(){
		public void actionPerformed(ActionEvent e){
			System.exit(1);
				}	
			});
	MenuItem iinitenv = new MenuItem("Initialisation environnement");  
	MenuItem imanthe = new MenuItem("Manuel Theorique de TritonAMR");  
	MenuItem imaninfo = new MenuItem("Manuel Informatique de TritonAMR");   

	menuPrinc1.add(iouvrir);  
	menuPrinc1.add(ienregistrer); 
	menuPrinc1.add(iquitter);
	menuPrinc2.add(iinitenv);
	menuPrinc3.add(imanthe);
	menuPrinc3.add(imaninfo);
	mbar.add(menuPrinc1);  
	mbar.add(menuPrinc2); 
	mbar.add(menuPrinc3); 
	setMenuBar(mbar); 

	JSeparator js2 = new JSeparator();
	js2.setBounds(0,380,1000,50);
        top.add(js2);

/**
*/

/**
	PARTIE DE CONSTRUCTION DES BOUTTONS DE COMMANDES 
**/
 	JButton bgeneralite = new JButton ("GENERALITES");
	bgeneralite.setBounds(20,400,200,80);
	bgeneralite.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
  		FrameGeneralite fgeneralite = new FrameGeneralite();
		}
	});
	top.add(bgeneralite);


 	JButton bdomaine = new JButton ("DOMAINE DE CALCUL");
	bdomaine.setBounds(230,400,200,80);
	bdomaine.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
 	FrameDomaine fdomaine = new FrameDomaine();
		}
	});
	top.add(bdomaine);

 	bdiscre = new JButton ("DISCRETISATION");
	bdiscre.setBounds(440,400,200,80);
   	bdiscre.setEnabled(false);
	bdiscre.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
 	FrameDiscre fd = new FrameDiscre();
		}
	});
	top.add(bdiscre);

 	bamr = new JButton ("ADAPTATION AUTOMATIQUE DE MAILLAGE");
	bamr.setBounds(650,400,340,80);
   	bamr.setEnabled(false);
	bamr.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
  		FrameAmr famr = new FrameAmr();
		}
	});
	top.add(bamr);

	bresol = new JButton ("SYSTEME D'EQUATION");
	bresol.setBounds(20,490,200,80);
	bresol.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
  		FrameSys fsys = new FrameSys();
		}
	});
	top.add(bresol);

 	bmeth = new JButton ("METHODE DE RESOLUTION");
	bmeth.setBounds(230,490,200,80);
     	bmeth.setEnabled(false);
	bmeth.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
 	FrameNumerique fnum = new FrameNumerique();
		}
	});
	top.add(bmeth);

 	bposttrait = new JButton ("POST-TRAITEMENT");
	bposttrait.setBounds(440,490,200,80);
   	bposttrait.setEnabled(false);
	bposttrait.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
 	FramePost fpost = new FramePost();
		}
	});
	top.add(bposttrait);

 	bgenejdd = new JButton ("GENERATION JDD");
	bgenejdd.setBounds(40,580,200,50);
	bgenejdd.setForeground(Color.BLUE);
     	bgenejdd.setEnabled(false);
 	bgenejdd.setIcon(new ImageIcon(getClass().getClassLoader().getResource("trace.png")));
	top.add(bgenejdd);
	bgenejdd.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			try
			{
			FileWriter out = new FileWriter("TritonAMR.data");

			out.write("#------ OPTION ------\n");
			out.write("DEBUG");
			out.write("\n");
			out.write("#-----------------------\n\n");

			out.write("#------ EQUATION DE TRANSPORT : FRACTION VOLUMIQUE ------\n");
			out.write("#--------------------------------------------------------\n");
			out.write("#------ TYPE DE SCHEMA NUMERIQUE ------\n");
			out.write(SC1);
			out.write(" ");
			out.write(DIR1);
			out.write("\n");
			out.write("#--------------------------------------\n\n");

			out.write("#------ DISCRETISATION ------\n");
			if(TEST9 == 1) out.write("DISC 2D ");
			if(TEST9 == 2) out.write("DISC 3D ");
			out.write(NB_NODES_X.getText());
			out.write(" ");
			out.write(NB_NODES_Y.getText());
			out.write(" ");
			out.write(NB_NODES_Z.getText());
			out.write("\n");
			out.write("#--------------------------------------\n\n");

			out.write("#------ DEFINITION DU DOMAINE DE CALCUL ------\n");
			out.write("GRID ");
			out.write(MIN_X.getText());
			out.write("  ");
			out.write(MAX_X.getText());
			out.write("  ");
			out.write(MIN_Y.getText());
			out.write("  ");
			out.write(MAX_Y.getText());
			out.write("  ");
			out.write(MIN_Z.getText());
			out.write("  ");
			out.write(MAX_Z.getText());
			out.write("\n");
			out.write("#--------------------------------------\n\n");


			out.write("#------ CONDITIONS AUX LIMITES ---------\n");
			if(TEST2 == 1)
			{
			out.write("BC "); 
			out.write(CL1);
			}
			if(TEST3 == 1 )
			{
			out.write("BC USER"); 
			}
			out.write("\n");
			out.write("#---------------------------------------\n\n");

			out.write("#----------------- INITIALISATION ------------------\n");
			if(TEST4 == 1)
			{
			out.write("INIT ");
			out.write(CI1);
			}
			if(TEST5 == 1)
			{
			out.write("INIT BULLE USER");
			}
			out.write("\n");
			if(EQ2_YES == 1)
			{
			out.write("INIT ADVECTION ELLIPTIQUE ");
			out.write(CH14);
			}else
			{
			if(TEST19 == 1 && TEST12 == 0)
			{
			out.write("INIT ADVECTION ");
			out.write(CH1);
			}
			if(TEST12 == 1)
			{
			out.write("INIT ADVECTION USER");
			}
			out.write("\n");
			if(TEST16 == 1 )
			{
			out.write("MISEAJOUR_ADVECTION PERIODE ");
			out.write(MAJ_PERIODE.getText());
			}
			}
			out.write("\n");
			out.write("#--------------------------------------------------\n\n");

			out.write("#------------ TEMPS MAX ------------\n");
			out.write("MAX_TEMPS ");
			out.write(T_MAX.getText());
			out.write("\n");
			out.write("#--------------------------------------\n\n");
			
			if (TEST6 == 2)
			{
			out.write("#----------- METHODE AMR --------------------\n");
			out.write("#--- METHODE 1: X____Y___Z - X____Y____Z ---\n");
			out.write("#--- METHODE 2: X____X - Y____Y - Z____Z ---\n");
			out.write("AMR METHODE 1");
			out.write("\n");
			out.write("#----------------------------------------------\n\n"); 

			out.write("#---- NIVEAU DE RAFFINEMENT MAXIMUM --------\n");
			out.write("AMR LMAX 0");
			out.write("\n");
			out.write("#----------------------------------------------\n\n"); 
			}

			if (TEST6 == 1)
			{
			out.write("#----------- METHODE AMR --------------------\n");
			out.write("#--- METHODE 1: X____Y___Z - X____Y____Z ---\n");
			out.write("#--- METHODE 2: X____X - Y____Y - Z____Z ---\n");
			out.write("AMR METHODE ");
			if(TEST20 == 1)
			{
 			out.write("1");
			}else if(TEST20 == 2)
 			{
			out.write("2");
			}
			out.write("\n");
			out.write("#----------------------------------------------\n\n"); 

			out.write("#---- TYPE AMR (LOCAL-GLOBAL) --------\n");
			out.write("AMR ");
			if(TEST21 == 1)
			{
 			out.write("LOCAL");
			}else if(TEST21 == 2)
 			{
			out.write("GLOBAL");
			}
			out.write("\n");
			out.write("#----------------------------------------------\n\n"); 

			out.write("#---- NIVEAU DE RAFFINEMENT MAXIMUM --------\n");
			out.write("AMR LMAX ");
			out.write(L_MAX.getText());
			out.write("\n");
			out.write("#----------------------------------------------\n\n"); 

			out.write("#---------- NOMBRE DE MAILLES DE SECURITE -------\n");
			out.write("AMR ISF ");
			out.write(ISF_MAX.getText());
			out.write("\n");
			out.write("#----------------------------------------------\n\n"); 

			out.write("#------------ COEFICIENT DE RAFINEMENT -------\n");
			out.write("AMR RI ");
			out.write(RC_X.getText());
			out.write("\n");
			out.write("AMR RJ ");
			out.write(RC_Y.getText());
			out.write("\n");
			out.write("AMR RK ");
			out.write(RC_Z.getText());
			out.write("\n");
			out.write("#----------------------------------------------\n\n"); 

			out.write("#------------ LONGUEUR MAX DES PATCHES -------\n");
			out.write("AMR DIMCUT ");
			out.write(DIMCUT_X.getText());
			out.write(" ");
			out.write(DIMCUT_Y.getText());
			out.write(" ");
			out.write(DIMCUT_Z.getText());
			out.write("\n");
			out.write("#----------------------------------------------\n\n"); 

			out.write("#----- TOLERANCE DE CRITERE DE RAFFINEMENT -----\n");
			out.write("AMR DELT ");
			if(TEST30 == 1)
			{
			out.write(TOL_SEN.getText());
			}else if(TEST30 == 2)
 			{
			out.write("USER");
			}
			out.write("\n");
			out.write("#----------------------------------------------\n\n"); 
			
			out.write("#----- TOLERANCE ALGORITHME GROUPING/CLUSTERING -----\n");
			out.write("AMR TOL ");
			out.write(TOL_GRCL.getText());
			out.write("\n");
			out.write("#----------------------------------------------\n\n"); 

			out.write("#----- FREQUENCE DE GENERATION DE MAILLAGES -----\n");
			out.write("AMR ITGFR ");
			out.write(F_AMR.getText());
			out.write("\n");
			out.write("#----------------------------------------------\n\n"); 
			}

			out.write("#-------- DELTA T OU CFL  -------\n");
			out.write("DELTA_T_OR_CFL ");
			if (TEST15 == 1 )
			{
			out.write("1 ");
			out.write(PAS_TEMPS.getText());
			}else if (TEST15 == 2)
			{ 
			out.write("2 ");
			out.write(CFL.getText());
			}
			out.write("\n");
			out.write("#--------------------------------------------------------\n\n");

			if(EQ2_YES == 1)
			{
			out.write("#-EQUATION DE LAPLACIEN ( PARTIE POTENTIELLE - BULLE ABSTRAITE ) -\n");
			out.write("#-----------------------------------------------------------------\n");
			out.write("#------ METHODE DE RESOLUTION ------\n");
			out.write("ELLIPTIQUE METH ");
			out.write(ME1);
			out.write("\n");
			out.write("#--------------------------------------\n\n");
			out.write("#------ NOMBRE D'ITERATIONS MAX POUR LDC ------\n");
			out.write("LDC ITERMAX ");
			out.write(ILDC_MAX.getText());
			out.write("\n");
			out.write("#--------------------------------------\n\n");
			out.write("#------ TOLERANCE DE CONVERGENCE POUR LDC ------\n");
			out.write("LDC TOL ");
			out.write(TLDC_MAX.getText());
			out.write("\n");
			out.write("#--------------------------------------\n\n");
			out.write("#------ NOMBRE D'ITERATIONS MAX POUR GC ------\n");
			out.write("LDC GCITERMAX ");
			out.write(IGC_MAX.getText());
			out.write("\n");
			out.write("#--------------------------------------\n\n");
			out.write("#------ TOLERANCE DE CONVERGENCE POUR GC ------\n");
			out.write("LDC GCTOL ");
			out.write(TGC_MAX.getText());
			out.write("\n");
			out.write("#--------------------------------------\n\n");
			out.write("#-----------------------------------------------------------------\n");
			}

			if (TEST23 == 1) 
			{
				out.write("#----- VISUALISATION PARAVIEW: FRACTION VOLUMIQUE -----\n");
				out.write("VISU PARAVIEW CHAMP Y ");	
				out.write(FREQ_FRACVOL.getText());
				out.write("\n");
				out.write("#-----------------------------------------------------\n\n"); 
			}		

			out.write("#----- NOMBRE D'ITERATIONS -----\n");
			out.write("RUN TritonAMR ");	
			out.write(I_MAX.getText());
			out.write("\n");
			out.write("#-----------------------------------------------------\n\n"); 

			out.write("#----- FIN DU JDD -----\n");
			out.write("END");	

			out.close();
			}
			catch(IOException ev)
			{
			System.err.println(ev.getMessage());
			}
	   		bdata.setEnabled(true);
  			brun.setEnabled(true);
			
		}
		});

 	bdata = new JButton ("JDD DATA");
	bdata.setBounds(280,580,200,50);
	bdata.setForeground(Color.GREEN);
   	bdata.setEnabled(false);
 	bdata.setIcon(new ImageIcon(getClass().getClassLoader().getResource("resume.png")));
	bdata.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
		try {
			Runtime.getRuntime().exec("kwrite ../DATA/TritonAMR.data");
                }
                catch(java.io.IOException e1) {
                    e1.printStackTrace();
                }
		}
		});
	top.add(bdata);

 	brun = new JButton ("EXECUTION");
	brun.setBounds(520,580,200,50);
	brun.setForeground(Color.RED);
   	brun.setEnabled(false);
 	brun.setIcon(new ImageIcon(getClass().getClassLoader().getResource("executer.png")));
	brun.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
		try {
			Runtime.getRuntime().exec("../SCRIPTS/Lance_TritonAMR");
                }
                catch(java.io.IOException e1) {
                    e1.printStackTrace();
                }
		}
		});
	top.add(brun);

 	bresume = new JButton ("RESUME CALCUL");
	bresume.setBounds(760,580,200,50);
	bresume.setForeground(Color.RED);
   	bresume.setEnabled(false);
 	bresume.setIcon(new ImageIcon(getClass().getClassLoader().getResource("resume.png")));
	bresume.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
		}
		});
	top.add(bresume);

/**
*/

        // On definit "top" comme etant le content pane principal de "MainFrame" et on le rends visible:
        this.setContentPane(top);
        this.setVisible(true);
        
    	}
		
	 private void ouvrirProjet() {
		String monfichier="";
        if (fileChooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {	
           monfichier = fileChooser.getSelectedFile().getAbsolutePath();
           System.out.println(monfichier);
          }
    }


public class FrameGeneralite
{
	private JButton b;				//bouton "ok"
	private JFrame fgener;
	public FrameGeneralite(){
		fgener = new JFrame();
		fgener.setSize(750, 500);
		//Nous allons maintenant dire à notre objet de se positionner au centre
		fgener.setLocationRelativeTo(null);
		//donner un nom � la frame
		fgener.setTitle("GENERALITES");	
		//on creer un container:
		JPanel panel4 = new JPanel();
		panel4.setLayout(null);

		{
		label_IF = new JLabel("--- INFORMATIONS GENERALES ---");
		label_IF.setBounds(300,30,400,25);
		panel4.add(label_IF);

		JLabel label_typec = new JLabel("TYPE DE CALCUL :");
		label_typec.setBounds(20,75,180,25);
		panel4.add(label_typec);

		// Créer un JComboBox
		JComboBox namesComboBox3 = new JComboBox();
		namesComboBox3.addItem(TC1);
		namesComboBox3.addItem(TC2);
 		namesComboBox3.addItem(TC3);
		namesComboBox3.setBounds(250,75,200,25);
		namesComboBox3.addActionListener(new ActionListener(){
		public void actionPerformed(ActionEvent e){
  				JComboBox cb = (JComboBox)e.getSource();	
				String selected = (String)cb.getSelectedItem();
				if (selected.equals("SEQUENTIELLE"))
				{
				   TC1 = "SEQUENTIELLE";
  				   TC2 = "CHOISISSEZ";
  				   TC3 = "PARALLELE";
				   TEST1 = 1;	
				}else if(selected.equals("PARALLELE"))
				{
  				   TC1 = "PARALLELE";
				   TC3 = "SEQUENTIELLE";
  				   TC2 = "CHOISISSEZ";
				   TEST1 = 1;	
				}else if(selected.equals("CHOISISSEZ"))
				{
  				   TC1 = "CHOISISSEZ";
				   TC3 = "SEQUENTIELLE";
  				   TC2 = "PARALLELE";
				   TEST1 = 0;	
				}
			}
  			});

		panel4.add(namesComboBox3);

		JLabel label_tmax = new JLabel("TEMPS PHYSIQUE MAX DE LA SIMULATION :");
		label_tmax.setBounds(20,130,300,25);
		panel4.add(label_tmax);

		T_MAX =  new JTextField();
		T_MAX.setBounds(350,130,100,25);

		//Mise en forme champs de texte 2ieme conversion:
		Font police = new Font("Arial", Font.BOLD, 14);
		T_MAX.setFont(police);
		T_MAX.setPreferredSize(new Dimension(100, 30));
		T_MAX.setForeground(Color.BLUE);

		T_MAX.setText(""+TMAX);
					
		panel4.add(T_MAX);

		JLabel label_inmax = new JLabel("NOMBRE D'ITERATIONS MAX :");
		label_inmax.setBounds(20,180,250,25);
		panel4.add(label_inmax);

		I_MAX =  new JTextField();
		I_MAX.setBounds(350,180,100,25);

		I_MAX.setFont(police);
		I_MAX.setPreferredSize(new Dimension(100, 30));
		I_MAX.setForeground(Color.BLUE);

		I_MAX.setText(""+IMAX);
					
		panel4.add(I_MAX);

		JLabel label_typebc = new JLabel("TYPE DES CONDITIONS AUX LIMITES :");
		label_typebc.setBounds(20,230,250,25);
		panel4.add(label_typebc);

		// Créer un JComboBox
		namesComboBox4 = new JComboBox();
		namesComboBox4.addItem(CL1);
		namesComboBox4.addItem(CL2);
		namesComboBox4.setBounds(280,230,200,25);
		namesComboBox4.addActionListener(new ActionListener(){
		public void actionPerformed(ActionEvent e){
  				JComboBox cb = (JComboBox)e.getSource();	
				String selected = (String)cb.getSelectedItem();
				if (selected.equals("PERIODIQUE"))
				{
				   CL1 = "PERIODIQUE";
  				   CL2 = "CHOISISSEZ";
				   TEST2 = 1;	
				}else if(selected.equals("CHOISISSEZ"))
				{
  				   CL1 = "CHOISISSEZ";
				   CL2 = "PERIODIQUE";
				   TEST2 = 0;	
				}
			}
  			});
		panel4.add(namesComboBox4);

		JCheckBox cbut = new JCheckBox("UTULISATEUR");
		cbut.setBounds(500,230,150,25);
		cbut.addItemListener(new ItemListener(){
		public void itemStateChanged(ItemEvent e){
				if (e.getStateChange() == ItemEvent.DESELECTED)
				{	
  				namesComboBox4.setEnabled(true);				
				ind_cbut = 1;
  			        TEST3 = 0;
				} else
				{
  				namesComboBox4.setEnabled(false);				
				ind_cbut = 2;
  			        TEST3 = 1;	
				TEST2 = 2;		
				}
 				}	
  			});
		panel4.add(cbut);

		if(TEST3 == 1)
		{
		cbut.setSelected(true);
		}

		JLabel label_typeci = new JLabel("CONDITIONS INITIALES :");
		label_typeci.setBounds(20,280,200,25);
		panel4.add(label_typeci);

		// Créer un JComboBox
		namesComboBox5 = new JComboBox();
		namesComboBox5.addItem(CI1);
		namesComboBox5.addItem(CI2);
		namesComboBox5.addItem(CI3);
		namesComboBox5.addItem(CI4);
		namesComboBox5.addItem(CI5);
		namesComboBox5.addItem(CI6);
		namesComboBox5.setBounds(250,280,200,25);
		namesComboBox5.addActionListener(new ActionListener(){
		public void actionPerformed(ActionEvent e){
  				JComboBox cb = (JComboBox)e.getSource();	
				String selected = (String)cb.getSelectedItem();
				if (selected.equals("BULLE CARREE"))
				{
  			           TEST4 = 1;	
				   CI1 = "BULLE CARREE";
  				   CI2 = "CHOISISSEZ";
  				   CI3 = "BULLE ZALESAK_I";
  				   CI4 = "BULLE CERCLE_KOTHE";
  				   CI5 = "BULLE SPHERE_KOTHE";
				   CI6 = "BULLE CUBE";
				}else if(selected.equals("BULLE ZALESAK_I"))
				{
  			           TEST4 = 1;	
				   CI1 = "BULLE ZALESAK_I";
  				   CI2 = "CHOISISSEZ";
  				   CI3 = "BULLE CARREE";
  				   CI4 = "BULLE CERCLE_KOTHE";
  				   CI5 = "BULLE SPHERE_KOTHE";
				   CI6 = "BULLE CUBE";
				}else if(selected.equals("BULLE CERCLE_KOTHE"))
				{
  			           TEST4 = 1;	
				   CI1 = "BULLE CERCLE_KOTHE";
  				   CI2 = "CHOISISSEZ";
  				   CI3 = "BULLE CARREE";
  				   CI4 = "BULLE ZALESAK_I";
  				   CI5 = "BULLE SPHERE_KOTHE";
				   CI6 = "BULLE CUBE";
				}else if(selected.equals("BULLE SPHERE_KOTHE"))
				{
  			           TEST4 = 1;	
				   CI1 = "BULLE SPHERE_KOTHE";
  				   CI2 = "CHOISISSEZ";
  				   CI3 = "BULLE CARREE";
  				   CI4 = "BULLE ZALESAK_I";
				   CI5 = "BULLE CERCLE_KOTHE";
				   CI6 = "BULLE CUBE";
				}else if(selected.equals("BULLE CUBE"))
				{
  			           TEST4 = 1;	
				   CI1 = "BULLE CUBE";
  				   CI2 = "CHOISISSEZ";
  				   CI3 = "BULLE CARREE";
  				   CI4 = "BULLE ZALESAK_I";
  				   CI5 = "BULLE CERCLE_KOTHE";
  				   CI6 = "BULLE SPHERE_KOTHE";
				}else if(selected.equals("CHOISISSEZ"))
				{
  			           TEST4 = 0;	
				   CI1 = "CHOISISSEZ";
  				   CI2 = "BULLE CARREE";
  				   CI3 = "BULLE ZALESAK_I";
  				   CI4 = "BULLE CERCLE_KOTHE";
  				   CI5 = "BULLE SPHERE_KOTHE";
				   CI6 = "BULLE CUBE";
				}
			}
  			});
		panel4.add(namesComboBox5);

		JCheckBox ciut = new JCheckBox("UTULISATEUR");
		ciut.setBounds(470,280,150,25);
		ciut.addItemListener(new ItemListener(){
		public void itemStateChanged(ItemEvent e){
				if (e.getStateChange() == ItemEvent.DESELECTED)
				{	
 		                TEST5 = 0;	
  				namesComboBox5.setEnabled(true);				
				ind_ciut = 1;
				} else
				{
 		                TEST5 = 1;	
 		                TEST4 = 2;	
  				namesComboBox5.setEnabled(false);				
				ind_ciut = 2;
				}
 				}	
  			});
		panel4.add(ciut);

		if(TEST5 == 1)
		{
		ciut.setSelected(true);
		}

		JLabel label_amr_on = new JLabel("ADAPTATION AUTMATIQUE DE MAILLAGE :");
		label_amr_on.setBounds(20,330,300,25);
		panel4.add(label_amr_on);

 		ButtonGroup bgroupamr1 = new ButtonGroup();
		JRadioButton cbamr1 = new JRadioButton("ACTIVE");
		cbamr1.setBounds(350,330,100,25);
		cbamr1.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e){
				bamr.setEnabled(true);
 		                TEST6 = 1;	
				}	
			});
		bgroupamr1.add(cbamr1);
		panel4.add(cbamr1);				
		JRadioButton cbamr2 = new JRadioButton("DESACTIVE");
		cbamr2.setBounds(450,330,100,25);
		cbamr2.addActionListener(new ActionListener(){
		public void actionPerformed(ActionEvent e){
				bamr.setEnabled(false);
 		                TEST6 = 2;	
 				}	
  			});
		bgroupamr1.add(cbamr2);
		panel4.add(cbamr2);				

		if(TEST6 == 1)
		{
		cbamr1.setSelected(true);
		}

		if(TEST6 == 2)
		{
		cbamr2.setSelected(true);
		}

		JLabel label_clip = new JLabel("CLIPPING :");
		label_clip.setBounds(20,380,300,25);
		panel4.add(label_clip);

 		ButtonGroup bgroupclip = new ButtonGroup();
		JRadioButton cbclip1 = new JRadioButton("ACTIVE");
		cbclip1.setBounds(350,380,100,25);
		cbclip1.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e){
 		                TEST7 = 1;	
				}	
			});
		bgroupclip.add(cbclip1);
		panel4.add(cbclip1);				
		JRadioButton cbclip2 = new JRadioButton("DESACTIVE");
		cbclip2.setBounds(450,380,100,25);
		cbclip2.addActionListener(new ActionListener(){
		public void actionPerformed(ActionEvent e){
 		                TEST7 = 2;	
 				}	
  			});
		bgroupclip.add(cbclip2);
		panel4.add(cbclip2);				

		if(TEST7 == 1)
		{
		cbclip1.setSelected(true);
		}
		if(TEST7 == 2)
		{
		cbclip2.setSelected(true);
		}

		}

		{
		JButton bOk = new JButton();
		bOk.setText("OK");
		bOk.setBounds(225, 430, 100, 30);
		bOk.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			if (TEST1 >= 1 && TEST2 >= 1 && TEST4 >= 1 && TEST6 >= 1 && TEST7 >= 1 ){
 			IMAX=Integer.parseInt(I_MAX.getText());
   			TMAX=Double.parseDouble(T_MAX.getText());
			OK1 = 1;
			if (TEST6 == 1)
			{
			if (OK2 == 1 && OK3 == 1 && OK4 == 1 && OK5 == 1 && OK6 == 1 && OK7 == 1){
   				bgenejdd.setEnabled(true);
 			}
			}
			if (TEST6 == 2)
			{
			if (OK2 == 1 && OK3 == 1 && OK4 == 1 && OK5 == 1 && OK7 == 1){
   				bgenejdd.setEnabled(true);
 			}
			}
				fgener.dispose();
 			}
				}
			});
		panel4.add(bOk);
		}

		{
		JButton bCancel = new JButton();
		bCancel.setText("ANNULER");
		bCancel.setBounds(425, 430, 100, 30);
		bCancel.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			fgener.dispose();
				}
			});
		panel4.add(bCancel);
		}
				
				

		fgener.setVisible(true);
		fgener.setIconImage(new ImageIcon(getClass().getClassLoader().getResource("about.png")).getImage());
		fgener.setContentPane(panel4);
	}
}

public class FrameDomaine
{
	private JButton b;
	private JFrame fdom;
	public JLabel jLabelminZ;
	public JLabel jLabelmaxZ;
	public FrameDomaine(){
		fdom = new JFrame();
		fdom.setSize(600, 570);
		//Nous allons maintenant dire à notre objet de se positionner au centre
		fdom.setLocationRelativeTo(null);
		//donner un nom � la frame
		fdom.setTitle("DOMAINE DE CALCUL");	
		//on creer un container:
		JPanel panel5 = new JPanel();
		panel5.setLayout(null);

		{
		JLabel label_EQ15 = new JLabel("--- CORDONNEES DU DOMAINE DE CALCUL ---");
		label_EQ15.setBounds(140,30,320,25);
		panel5.add(label_EQ15);

		JLabel jLabel15 = new JLabel();
		jLabel15.setText("DIMENSION :");
		jLabel15.setBounds(20, 105, 100, 25);
		panel5.add(jLabel15);

 		ButtonGroup bgroup15 = new ButtonGroup();
		JRadioButton cb15 = new JRadioButton("2D");
		cb15.setBounds(175,105,50,25);
		cb15.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e){
  				MIN_X.setEnabled(true);				
  				MIN_Y.setEnabled(true);				
  				MIN_Z.setVisible(false);				
  				MAX_X.setEnabled(true);				
  				MAX_Y.setEnabled(true);				
  				MAX_Z.setVisible(false);
				jLabelminZ.setVisible(false);
				jLabelmaxZ.setVisible(false);
				NB_NODES_Z_YES = 1;
				RC_Z_YES = 1;
				DIMCUT_Z_YES = 1;
				TEST9=1;
				}	
			});
		bgroup15.add(cb15);
		panel5.add(cb15);				
		JRadioButton cb25 = new JRadioButton("3D");
		cb25.setBounds(425,105,50,25);
		cb25.addActionListener(new ActionListener(){
		public void actionPerformed(ActionEvent e){
  				MIN_X.setEnabled(true);				
  				MIN_Y.setEnabled(true);				
  				MIN_Z.setVisible(true);				
  				MAX_X.setEnabled(true);				
  				MAX_Y.setEnabled(true);				
  				MAX_Z.setVisible(true);				
				jLabelminZ.setVisible(true);
				jLabelmaxZ.setVisible(true);
				NB_NODES_Z_YES = 2;
				RC_Z_YES = 2;
				DIMCUT_Z_YES = 2;
				TEST9 = 2;
 				}	
  			});
		bgroup15.add(cb25);
		panel5.add(cb25);

		if(TEST9 == 1)
		{
		cb15.setSelected(true);
		}
		if(TEST9 == 2)
		{
		cb25.setSelected(true);
		}
				
		}

		{				
		JLabel jLabelcord = new JLabel();
		jLabelcord.setText("CORDONNEES :");
		jLabelcord.setBounds(20, 150, 100, 25);
		panel5.add(jLabelcord);
		MIN_X = new JTextField();
		MIN_Y = new JTextField();     
		MIN_Z = new JTextField();     
		MAX_X = new JTextField();
		MAX_Y = new JTextField();
		MAX_Z = new JTextField();     

		JLabel jLabelmin = new JLabel();
		jLabelmin.setText("MIN");
		jLabelmin.setBounds(210, 150, 50, 25);
		panel5.add(jLabelmin);

		JLabel jLabelmax = new JLabel();
		jLabelmax.setText("MAX");
		jLabelmax.setBounds(360, 150, 50, 25);
		panel5.add(jLabelmax);

		JLabel jLabelminX = new JLabel();
		jLabelminX.setText("X");
		jLabelminX.setBounds(175, 175, 25, 25);
		panel5.add(jLabelminX);
		JLabel jLabelmaxX = new JLabel();
		jLabelmaxX.setText("X");
		jLabelmaxX.setBounds(325, 175, 25, 25);
		panel5.add(jLabelmaxX);

		JLabel jLabelminY = new JLabel();
		jLabelminY.setText("Y");
		jLabelminY.setBounds(175, 225, 25, 25);
		panel5.add(jLabelminY);
		JLabel jLabelmaxY = new JLabel();
		jLabelmaxY.setText("Y");
		jLabelmaxY.setBounds(325, 225, 25, 25);
		panel5.add(jLabelmaxY);

		jLabelminZ = new JLabel();
		jLabelminZ.setText("Z");
		jLabelminZ.setBounds(175, 275, 25, 25);
		panel5.add(jLabelminZ);
		jLabelmaxZ = new JLabel();
		jLabelmaxZ.setText("Z");
		jLabelmaxZ.setBounds(325, 275, 25, 25);
		panel5.add(jLabelmaxZ);

		MIN_X.setBounds(200,175,80,25);
		MIN_Y.setBounds(200,225,80,25);
		MIN_Z.setBounds(200,275,80,25);
		MAX_X.setBounds(350,175,80,25);
		MAX_Y.setBounds(350,225,80,25);
		MAX_Z.setBounds(350,275,80,25);
		Font police = new Font("Arial", Font.BOLD, 14);
		MIN_X.setFont(police);
		MIN_X.setPreferredSize(new Dimension(80, 25));
		MIN_X.setForeground(Color.BLUE);

		MIN_Y.setFont(police);
		MIN_Y.setPreferredSize(new Dimension(80, 25));
		MIN_Y.setForeground(Color.BLUE);

		MIN_Z.setFont(police);
		MIN_Z.setPreferredSize(new Dimension(80, 25));
		MIN_Z.setForeground(Color.BLUE);

		MAX_X.setFont(police);
		MAX_X.setPreferredSize(new Dimension(80, 25));
		MAX_X.setForeground(Color.BLUE);

		MAX_Y.setFont(police);
		MAX_Y.setPreferredSize(new Dimension(80, 25));
		MAX_Y.setForeground(Color.BLUE);

		MAX_Z.setFont(police);
		MAX_Z.setPreferredSize(new Dimension(80, 25));
		MAX_Z.setForeground(Color.BLUE);

		MIN_X.setText(""+MINX);
		MIN_Y.setText(""+MINY);
		MIN_Z.setText(""+MINZ);
		MAX_X.setText(""+MAXX);
		MAX_Y.setText(""+MAXY);
		MAX_Z.setText(""+MAXZ);

		panel5.add(MIN_X);
		panel5.add(MIN_Y);
		panel5.add(MIN_Z);
		panel5.add(MAX_X);
		panel5.add(MAX_Y);
		panel5.add(MAX_Z);


		if(TEST9 == 1)
		{
		MIN_Z.setVisible(false);
		MAX_Z.setVisible(false);
		jLabelminZ.setVisible(false);
		jLabelmaxZ.setVisible(false);
		}

		}
		
		{
			ImageIcon im11 = new ImageIcon("carree.jpg");
			JLabel toto11 = new JLabel(im11);
 			toto11.setBounds(100,310,150,150);
			panel5.add(toto11);
		}
		

		{
			ImageIcon im22 = new ImageIcon("cube.jpg");
			JLabel toto22 = new JLabel(im22);
 			toto22.setBounds(350,310,150,150);
			panel5.add(toto22);
		}

		{
		JButton bOk = new JButton();
		bOk.setText("OK");
		bOk.setBounds(150, 480, 100, 30);
		bOk.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			if (TEST9 >= 1)
			{
   			MINX=Double.parseDouble(MIN_X.getText());
   			MINY=Double.parseDouble(MIN_Y.getText());
   			MINZ=Double.parseDouble(MIN_Z.getText());
   			MAXX=Double.parseDouble(MAX_X.getText());
   			MAXY=Double.parseDouble(MAX_Y.getText());
   			MAXZ=Double.parseDouble(MAX_Z.getText());
   			bdiscre.setEnabled(true);
			OK2 = 1;

			if (TEST6 == 1)
			{
			if (OK1 == 1 && OK3 == 1 && OK4 == 1 && OK5 == 1 && OK6 == 1 && OK7 == 1){
   				bgenejdd.setEnabled(true);
 			}
			}
			if (TEST6 == 2)
			{
			if (OK1 == 1 && OK3 == 1 && OK4 == 1 && OK5 == 1 && OK7 == 1){
   				bgenejdd.setEnabled(true);
 			}
			}
			fdom.dispose();
			}
				}
			});
		panel5.add(bOk);
		}
		
		{
		JButton bCancel = new JButton();
		bCancel.setText("ANNULER");
		bCancel.setBounds(350, 480, 100, 30);
		bCancel.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			fdom.dispose();
				}
			});
		panel5.add(bCancel);
		}
				
		fdom.setVisible(true);
		fdom.setIconImage(new ImageIcon(getClass().getClassLoader().getResource("about.png")).getImage());
		fdom.setContentPane(panel5);
	}
}

public class FrameDiscre
{
	private JFrame fdiscre;
	public FrameDiscre(){
		fdiscre = new JFrame();
		fdiscre.setSize(500, 200);
		//Nous allons maintenant dire à notre objet de se positionner au centre
		fdiscre.setLocationRelativeTo(null);
		//donner un nom � la frame
		fdiscre.setTitle("DISCRETISATION DU DOMAINE DE CALCUL");	
		//on creer un container:
		JPanel panel2 = new JPanel();
		panel2.setLayout(null);
				
														{
		JLabel jLabel1 = new JLabel();
		fdiscre.getContentPane().add(jLabel1);
		jLabel1.setText("--- MAILLAGE GROSSIER ---");
		jLabel1.setBounds(150, 35, 200, 25);
		panel2.add(jLabel1);
		}

		{
 		JLabel label_nbmailles = new JLabel(" NOMBRE DE NOUEDS (X / Y / Z) :");
	 	label_nbmailles.setBounds(20,90,250,25);
		panel2.add(label_nbmailles);

		NB_NODES_X = new JTextField();
		NB_NODES_Y = new JTextField();     
		NB_NODES_Z = new JTextField();
		NB_NODES_X.setBounds(300,90,50,25);
		NB_NODES_Y.setBounds(360,90,50,25);
		NB_NODES_Z.setBounds(420,90,50,25);

		Font police = new Font("Arial", Font.BOLD, 14);
		NB_NODES_X.setFont(police);
		NB_NODES_X.setPreferredSize(new Dimension(50, 30));
		NB_NODES_X.setForeground(Color.BLUE);
			
		//2ieme champs de texte:
		NB_NODES_Y.setFont(police);
		NB_NODES_Y.setPreferredSize(new Dimension(50, 30));
		NB_NODES_Y.setForeground(Color.BLUE);
				
		//3ieme champs de texte:
		NB_NODES_Z.setFont(police);
		NB_NODES_Z.setPreferredSize(new Dimension(50, 30));
		NB_NODES_Z.setForeground(Color.BLUE);

		NB_NODES_X.setText(""+NX);
		NB_NODES_Y.setText(""+NY);
		NB_NODES_Z.setText(""+NZ);
		panel2.add(NB_NODES_X);
		panel2.add(NB_NODES_Y);
		if (NB_NODES_Z_YES == 1){
			NB_NODES_Z.setVisible(false);
		}else
		{
			NB_NODES_Z.setVisible(true);
		}

		panel2.add(NB_NODES_Z);

		}

		{
		JButton bOk = new JButton();
		bOk.setText("OK");
		bOk.setBounds(100, 130, 100, 30);
		bOk.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
 			NX=Integer.parseInt(NB_NODES_X.getText());
 			NY=Integer.parseInt(NB_NODES_Y.getText());
 			NZ=Integer.parseInt(NB_NODES_Z.getText());
			OK3 = 1;
			if(TEST6 == 1)
			{
			if (OK1 == 1 && OK2 == 1 && OK4 == 1 && OK5 == 1 && OK6 == 1 && OK7 == 1){
   				bgenejdd.setEnabled(true);
 			}
			}
			if(TEST6 == 2)
			{
			if (OK1 == 1 && OK2 == 1 && OK4 == 1 && OK5 == 1 && OK7 == 1){
   				bgenejdd.setEnabled(true);
 			}
			}
			fdiscre.dispose();
				}
			});
		panel2.add(bOk);
		}

		{
		JButton bCancel = new JButton();
		bCancel.setText("ANNULER");
		bCancel.setBounds(300, 130, 100, 30);
		bCancel.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			fdiscre.dispose();
				}
			});
		panel2.add(bCancel);
		}
				
				

		fdiscre.setVisible(true);
		fdiscre.setIconImage(new ImageIcon(getClass().getClassLoader().getResource("about.png")).getImage());
		fdiscre.setContentPane(panel2);
	}
}
	
public class FrameNumerique
{
	private JButton b;
	private JFrame fnum;
	public FrameNumerique(){
		fnum = new JFrame();
		fnum.setSize(750, 550);
		//Nous allons maintenant dire à notre objet de se positionner au centre
		fnum.setLocationRelativeTo(null);
		//donner un nom � la frame
		fnum.setTitle("METHODE NUMERIQUE DE RESOLUTION");	
		//on creer un container:
		JPanel panel3 = new JPanel();
		panel3.setLayout(null);
				
														{


		label_EQ1 = new JLabel("--- EQUATION DE TRANSPORT DE LA FRACTION VOLUMIQUE ---");
		label_EQ1.setBounds(150,30,450,25);
		panel3.add(label_EQ1);

		JLabel label_scheme = new JLabel("SCHEMA NUMERIQUE :");
		label_scheme.setBounds(20,75,180,25);
		panel3.add(label_scheme);

		JComboBox namesComboBox1 = new JComboBox();
		namesComboBox1.addItem(SC1);
		namesComboBox1.addItem(SC2);
		namesComboBox1.addItem(SC3);
		namesComboBox1.setBounds(250,75,200,25);
		namesComboBox1.addActionListener(new ActionListener(){
		public void actionPerformed(ActionEvent e){
  				JComboBox cb = (JComboBox)e.getSource();	
				String selected = (String)cb.getSelectedItem();
				if (selected.equals("SCHEMA LAGOUTIERE"))
				{
				   SC1 = "SCHEMA LAGOUTIERE";
  				   SC2 = "CHOISISSEZ";
  				   SC3 = "SCHEMA UPWIND";
				   TEST13 = 1;
				}else if(selected.equals("SCHEMA UPWIND"))
				{
  				   SC1 = "SCHEMA UPWIND";
  				   SC2 = "CHOISISSEZ";
				   SC3 = "SCHEMA LAGOUTIERE";
				   TEST13 = 1;
				}else if(selected.equals("CHOISISSEZ"))
				{
  				   SC1 = "CHOISISSEZ";
				   SC2 = "SCHEMA LAGOUTIERE";
  				   SC3 = "SCHEMA UPWIND";
				   TEST13 = 0;
				}
			}
  			});
		panel3.add(namesComboBox1);
	
		JComboBox namesComboBox2 = new JComboBox();
		namesComboBox2.addItem(DIR1);
		namesComboBox2.addItem(DIR2);
		namesComboBox2.addItem(DIR3);
		namesComboBox2.setBounds(500,75,200,25);
		namesComboBox2.addActionListener(new ActionListener(){
		public void actionPerformed(ActionEvent e){
  				JComboBox cb = (JComboBox)e.getSource();	
				String selected = (String)cb.getSelectedItem();
				if (selected.equals("DIRECTION_ALTERNEE"))
				{
				   DIR1 = "DIRECTION_ALTERNEE";
  				   DIR2 = "TYPE DE DIRECTION";
  				   DIR3 = "DIRECTION_NON_ALTERNEE";
				   TEST14 = 1;
				}else if(selected.equals("DIRECTION_NON_ALTERNEE"))
				{
  				   DIR1 = "DIRECTION_NON_ALTERNEE";
  				   DIR2 = "CHOISISSEZ";
				   DIR3 = "DIRECTION_ALTERNEE";
				   TEST14 = 1;
				}else if(selected.equals("TYPE DE DIRECTION"))
				{
  				   DIR1 = "TYPE DE DIRECTION";
				   DIR2 = "DIRECTION_ALTERNEE";
  				   DIR3 = "DIRECTION_NON_ALTERNEE";
				   TEST14 = 0;
				}
			}
  			});
		panel3.add(namesComboBox2);


		JLabel label_dt = new JLabel("PAS DE TEMPS / CFL :");
		label_dt.setBounds(20,130,180,25);
		panel3.add(label_dt);

 		ButtonGroup bgroup = new ButtonGroup();
		JRadioButton cb11 = new JRadioButton(" DELTA T");
		cb11.setBounds(250,130,100,25);
		cb11.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e){
				CFL.setVisible(false);
				PAS_TEMPS.setVisible(true);
				TEST15 = 1;
				}	
			});
		bgroup.add(cb11);
		panel3.add(cb11);				
		JRadioButton cb22 = new JRadioButton("CFL");
		cb22.setBounds(500,130,100,25);
		cb22.addActionListener(new ActionListener(){
		public void actionPerformed(ActionEvent e){
				PAS_TEMPS.setVisible(false);
				CFL.setVisible(true);
				TEST15 = 2;
 				}	
  			});
		bgroup.add(cb22);
		panel3.add(cb22);				

		PAS_TEMPS =  new JTextField();
		CFL = new JTextField();     
		PAS_TEMPS.setBounds(350,130,80,25);
		CFL.setBounds(600,130,80,25);

		Font police = new Font("Arial", Font.BOLD, 14);
		PAS_TEMPS.setFont(police);
		PAS_TEMPS.setPreferredSize(new Dimension(80, 25));
		PAS_TEMPS.setForeground(Color.BLUE);
			
		//2ieme champs de texte:
		CFL.setFont(police);
		CFL.setPreferredSize(new Dimension(80, 25));
		CFL.setForeground(Color.BLUE);
		
		PAS_TEMPS.setText(""+DT);
		CFL.setText(""+CFLD);

		PAS_TEMPS.setVisible(false);
		CFL.setVisible(false);
		panel3.add(PAS_TEMPS);
		panel3.add(CFL);
		if(TEST15 == 1)
		{
			PAS_TEMPS.setVisible(true);
			CFL.setVisible(false);
			cb11.setSelected(true);
		}
		if(TEST15 == 2)
		{
			PAS_TEMPS.setVisible(false);
			CFL.setVisible(true);
			cb22.setSelected(true);
		}

		}	
		{ 	
		label_EQ2 = new JLabel("--- EQUATION DE LAPLACIEN ( PARTIE POTENTIELLE - BULLE ABSTRAITE ) ---");
		label_EQ2.setBounds(130,180,490,25);
		if (EQ2_YES == 0)
		{
		 label_EQ2.setVisible(false);
		}else
		{
		 label_EQ2.setVisible(true);
		}
		panel3.add(label_EQ2);

		label_scheme2 = new JLabel("METHODE RESOLUTION :");
		label_scheme2.setBounds(20,220,180,25);
		if (EQ2_YES == 0)
		{
		label_scheme2.setVisible(false);
		}else
		{
		label_scheme2.setVisible(true);
		}
		panel3.add(label_scheme2);

		namesComboBox21 = new JComboBox();
		namesComboBox21.addItem(ME1);
		namesComboBox21.addItem(ME2);
		namesComboBox21.setBounds(250,220,200,25);
		namesComboBox21.addActionListener(new ActionListener(){
		public void actionPerformed(ActionEvent e){
  				JComboBox cb = (JComboBox)e.getSource();	
				String selected = (String)cb.getSelectedItem();
				if (selected.equals("LDC"))
				{
					ME1 = "LDC";
					ME2 = "CHOISISSEZ";
					label_ildc.setVisible(true);
					label_tldc.setVisible(true);
					ILDC_MAX.setVisible(true);
					TLDC_MAX.setVisible(true);
					label_igc.setVisible(true);
					label_tgc.setVisible(true);
					IGC_MAX.setVisible(true);
					TGC_MAX.setVisible(true);
					TEST8 = 1;
				}else if (selected.equals("ChOISISSEZ"))
				{
					ME2 = "LDC";
					ME1 = "CHOISISSEZ";
					label_ildc.setVisible(false);
					label_tldc.setVisible(false);
					ILDC_MAX.setVisible(false);
					TLDC_MAX.setVisible(false);
					label_igc.setVisible(false);
					label_tgc.setVisible(false);
					IGC_MAX.setVisible(false);
					TGC_MAX.setVisible(false);
					TEST8 = 0;
				} 
			}
  			});
		if (EQ2_YES == 0)
		{
		namesComboBox21.setVisible(false);
		}else
		{
		namesComboBox21.setVisible(true);
		}
		panel3.add(namesComboBox21);

		label_ildc = new JLabel("NOMBRE D'ITERATIONS MAX LDC :");
		label_ildc.setBounds(20,260,300,25);
		panel3.add(label_ildc);

		ILDC_MAX =  new JTextField();
		ILDC_MAX.setBounds(350,260,100,25);

		//Mise en forme champs de texte 2ieme conversion:
		Font police = new Font("Arial", Font.BOLD, 14);
		ILDC_MAX.setFont(police);
		ILDC_MAX.setPreferredSize(new Dimension(100, 30));
		ILDC_MAX.setForeground(Color.BLUE);

 		ILDC_MAX.setText(""+ILDCMAX);
					
		label_ildc.setVisible(false);
		ILDC_MAX.setVisible(false);
 		panel3.add(ILDC_MAX);					

		label_tldc = new JLabel("TOLERANCE LDC :");
		label_tldc.setBounds(20,300,300,25);
		panel3.add(label_tldc);

		TLDC_MAX =  new JTextField();
		TLDC_MAX.setBounds(350,300,100,25);

		//Mise en forme champs de texte 2ieme conversion:
		TLDC_MAX.setFont(police);
		TLDC_MAX.setPreferredSize(new Dimension(100, 30));
		TLDC_MAX.setForeground(Color.BLUE);

 		TLDC_MAX.setText(""+TLDCMAX);
					
		label_tldc.setVisible(false);
		TLDC_MAX.setVisible(false);
 		panel3.add(TLDC_MAX);					

		label_igc = new JLabel("NOMBRE D'ITERATIONS MAX GC :");
		label_igc.setBounds(20,340,300,25);
		panel3.add(label_igc);

		IGC_MAX =  new JTextField();
		IGC_MAX.setBounds(350,340,100,25);

		//Mise en forme champs de texte 2ieme conversion:
		IGC_MAX.setFont(police);
		IGC_MAX.setPreferredSize(new Dimension(100, 30));
		IGC_MAX.setForeground(Color.BLUE);

 		IGC_MAX.setText(""+IGCMAX);
					
		label_igc.setVisible(false);
		IGC_MAX.setVisible(false);
 		panel3.add(IGC_MAX);					


		label_tgc = new JLabel("TOLERANCE GC :");
		label_tgc.setBounds(20,380,300,25);
		panel3.add(label_tgc);

		TGC_MAX =  new JTextField();
		TGC_MAX.setBounds(350,380,100,25);

		//Mise en forme champs de texte 2ieme conversion:
		TGC_MAX.setFont(police);
		TGC_MAX.setPreferredSize(new Dimension(100, 30));
		TGC_MAX.setForeground(Color.BLUE);

 		TGC_MAX.setText(""+TGCMAX);
					
		label_tgc.setVisible(false);
		TGC_MAX.setVisible(false);
 		panel3.add(TGC_MAX);					

		if (TEST10 == 1 && TEST8 == 1)
		{
		label_ildc.setVisible(true);
		label_tldc.setVisible(true);
		ILDC_MAX.setVisible(true);
		TLDC_MAX.setVisible(true);
		label_igc.setVisible(true);
		label_tgc.setVisible(true);
		IGC_MAX.setVisible(true);
		TGC_MAX.setVisible(true);
		}

		}

		{
		JButton bOk = new JButton();
		bOk.setText("OK");
		bOk.setBounds(225, 480, 100, 30);
		bOk.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			if (TEST10 >= 1)
			{
			if (TEST13 >= 1 && TEST14 >= 1 && TEST8 >= 1){
   			DT=Double.parseDouble(PAS_TEMPS.getText());
   			CFLD=Double.parseDouble(CFL.getText());
   			ILDCMAX=Integer.parseInt(ILDC_MAX.getText());
   			IGCMAX=Integer.parseInt(IGC_MAX.getText());
   			TLDCMAX=Double.parseDouble(TLDC_MAX.getText());
   			TGCMAX=Double.parseDouble(TGC_MAX.getText());
			OK4 = 1;
			if(TEST6 == 1)
			{
			if (OK1 == 1 && OK3 == 1 && OK2 == 1 && OK5 == 1 && OK6 == 1 && OK7 == 1){
   				bgenejdd.setEnabled(true);
 			}
			}
			if(TEST6 == 2)
			{
			if (OK1 == 1 && OK3 == 1 && OK2 == 1 && OK5 == 1 && OK7 == 1){
   				bgenejdd.setEnabled(true);
 			}
			}
				fnum.dispose();
 			}
			}else
			{
			if (TEST13 >= 1 && TEST14 >= 1 && TEST15 >= 1 && TEST19 >= 1 ){
   			DT=Double.parseDouble(PAS_TEMPS.getText());
   			CFLD=Double.parseDouble(CFL.getText());
   			ILDCMAX=Integer.parseInt(ILDC_MAX.getText());
   			IGCMAX=Integer.parseInt(IGC_MAX.getText());
   			TLDCMAX=Double.parseDouble(TLDC_MAX.getText());
   			TGCMAX=Double.parseDouble(TGC_MAX.getText());
			OK4 = 1;
			if(TEST6 == 1)
			{
			if (OK1 == 1 && OK3 == 1 && OK2 == 1 && OK5 == 1 && OK6 == 1 && OK7 == 1){
   				bgenejdd.setEnabled(true);
 			}
			}
			if(TEST6 == 2)
			{
			if (OK1 == 1 && OK3 == 1 && OK2 == 1 && OK5 == 1 && OK7 == 1){
   				bgenejdd.setEnabled(true);
 			}
			}
				fnum.dispose();
 			}
			}
			}
			});
		panel3.add(bOk);
		}

		{
		JButton bCancel = new JButton();
		bCancel.setText("ANNULER");
		bCancel.setBounds(425, 480, 100, 30);
		bCancel.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
				fnum.dispose();
				}
			});
		panel3.add(bCancel);
		}
				
				
		fnum.setVisible(true);
		fnum.setIconImage(new ImageIcon(getClass().getClassLoader().getResource("about.png")).getImage());
		fnum.setContentPane(panel3);
	}
}


public class FrameSys
{
	private JFrame fsys;
	public FrameSys(){
		fsys = new JFrame();
		fsys.setSize(800, 400);
		//Nous allons maintenant dire à notre objet de se positionner au centre
		fsys.setLocationRelativeTo(null);
		//donner un nom � la frame
		fsys.setTitle("SYSTEME D'EQUATIONS");	
		//on creer un container:
		JPanel panel2 = new JPanel();
		panel2.setLayout(null);
				
		JLabel label_sys = new JLabel("--- SYSTEME D'EQUATIONS A RESOUDRE ---");
		label_sys.setBounds(150,30,300,25);
		panel2.add(label_sys);
		
		{
		JCheckBox eq1 = new JCheckBox("EQUATION DE TRANSPORT DE LA FRACTION VOLUMIQUE");
		eq1.setBounds(20,80,400,25);
		eq1.setSelected(true);
		eq1.setEnabled(false);
		panel2.add(eq1);
			
		label_chvi = new JLabel("CHAMP DE VITESSE :");
		label_chvi.setBounds(50,125,250,25);
	 	label_chvi.setVisible(true);
		panel2.add(label_chvi);

		// Créer un JComboBox
		namesComboBox14 = new JComboBox();
		namesComboBox14.addItem(CH1);
		namesComboBox14.addItem(CH2);
		namesComboBox14.addItem(CH3);
		namesComboBox14.addItem(CH4);
		namesComboBox14.addItem(CH5);
		namesComboBox14.addItem(CH6);
		namesComboBox14.setBounds(250,125,180,25);
		namesComboBox14.addActionListener(new ActionListener(){
		public void actionPerformed(ActionEvent e){
  				JComboBox cb = (JComboBox)e.getSource();	
				String selected = (String)cb.getSelectedItem();
				if (selected.equals("DIRECTION_X"))
				{
				   chut2.setVisible(false);
				   CH1 = "DIRECTION_X";
  				   CH2 = "CHOISISSEZ";
  				   CH3 = "DIRECTION_Y";
  				   CH4 = "DIRECTION_Z";
  				   CH5 = "DIRECTION_DIAG";
  				   CH6 = "DIRECTION_KOTHE_RIDER";
				   TEST11 = 0;
				   TEST19 = 1;
				}else if (selected.equals("DIRECTION_Y"))
				{
				   chut2.setVisible(false);
				   CH1 = "DIRECTION_Y";
  				   CH2 = "CHOISISSEZ";
  				   CH3 = "DIRECTION_X";
  				   CH4 = "DIRECTION_Z";
  				   CH5 = "DIRECTION_DIAG";
  				   CH6 = "DIRECTION_KOTHE_RIDER";
				   TEST11 = 0;
				   TEST19 = 1;
				}else if (selected.equals("DIRECTION_Z"))
				{
				   chut2.setVisible(false);
				   CH1 = "DIRECTION_Z";
  				   CH2 = "CHOISISSEZ";
  				   CH3 = "DIRECTION_X";
  				   CH4 = "DIRECTION_Y";
  				   CH5 = "DIRECTION_DIAG";
  				   CH6 = "DIRECTION_KOTHE_RIDER";
				   TEST11 = 0;
				   TEST19 = 1;
				}else if (selected.equals("DIRECTION_DIAG"))
				{
				   chut2.setVisible(true);
  				   CH1 = "DIRECTION_DIAG";
  				   CH2 = "CHOISISSEZ";
  				   CH3 = "DIRECTION_X";
  				   CH4 = "DIRECTION_Y";
				   CH5 = "DIRECTION_Z";
  				   CH6 = "DIRECTION_KOTHE_RIDER";
				   TEST11 = 0;
				   TEST19 = 1;
				}else if (selected.equals("DIRECTION_KOTHE_RIDER"))
				{
				   chut2.setVisible(true);
  				   CH1 = "DIRECTION_KOTHE_RIDER";
  				   CH2 = "CHOISISSEZ";
  				   CH3 = "DIRECTION_X";
  				   CH4 = "DIRECTION_Y";
				   CH5 = "DIRECTION_Z";
  				   CH6 = "DIRECTION_DIAG";
				   TEST11 = 1;
				   TEST19 = 1;
				}else if(selected.equals("CHOISISSEZ"))
				{
				   chut2.setVisible(false);
  				   CH1 = "CHOISISSEZ";
				   CH2 = "DIRECTION_X";
  				   CH3 = "DIRECTION_Y";
  				   CH4 = "DIRECTION_Z";
  				   CH5 = "DIRECTION_DIAG";
  				   CH6 = "DIRECTION_KOTHE_RIDER";
				   TEST11 = 0;
				   TEST19 = 0;
				}
			}
  			});

		// Créer un JComboBox
		namesComboBox144 = new JComboBox();
		namesComboBox144.addItem(CH14);
		namesComboBox144.addItem(CH24);
		namesComboBox144.addItem(CH34);
		namesComboBox144.addItem(CH44);
		namesComboBox144.addItem(CH54);
		namesComboBox144.addItem(CH64);
		namesComboBox144.setBounds(250,125,250,25);
		namesComboBox144.addActionListener(new ActionListener(){
		public void actionPerformed(ActionEvent e){
  				JComboBox cb = (JComboBox)e.getSource();	
				String selected = (String)cb.getSelectedItem();
				if (selected.equals("DIRECTION_DILATATION_POS"))
				{
				   CH14 = "DIRECTION_DILATATION_POS";
  				   CH24 = "CHOISISSEZ";
  				   CH34 = "DIRECTION_DILATATION_NEG";
  				   CH44 = "DIRECTION_DILATATION_OSCILLATIONS";
  				   CH54 = "DIRECTION_RESONANCE";
  				   CH64 = "DIRECTION_DILATATION_POS_NEG";
				   TEST29 = 1;
				}else if (selected.equals("DIRECTION_DILATATION_NEG"))
				{
				   CH14 = "DIRECTION_DILATATION_NEG";
  				   CH24 = "CHOISISSEZ";
  				   CH34 = "DIRECTION_DILATATION_POS";
  				   CH44 = "DIRECTION_DILATATION_OSCILLATIONS";
  				   CH54 = "DIRECTION_RESONANCE";
  				   CH64 = "DIRECTION_DILATATION_POS_NEG";
				   TEST29 = 1;
				}else if (selected.equals("DIRECTION_DILATATION_OSCILLATIONS"))
				{
				   CH14 = "DIRECTION_DILATATION_OSCILLATIONS";
  				   CH24 = "CHOISISSEZ";
  				   CH34 = "DIRECTION_DILATATION_POS";
  				   CH44 = "DIRECTION_DILATATION_NEG";
  				   CH54 = "DIRECTION_RESONANCE";
  				   CH64 = "DIRECTION_DILATATION_POS_NEG";
				   TEST29 = 1;
				}else if (selected.equals("DIRECTION_RESONANCE"))
				{
  				   CH14 = "DIRECTION_RESONANCE";
  				   CH24 = "CHOISISSEZ";
  				   CH34 = "DIRECTION_DILATATION_POS";
  				   CH44 = "DIRECTION_DILATATION_NEG";
				   CH54 = "DIRECTION_DILATATION_OSCILLATIONS";
  				   CH64 = "DIRECTION_DILATATION_POS_NEG";
				   TEST29 = 1;
				}else if (selected.equals("DIRECTION_DILATATION_POS_NEG"))
				{
  				   CH14 = "DIRECTION_DILATATION_POS_NEG";
  				   CH24 = "CHOISISSEZ";
  				   CH34 = "DIRECTION_DILATATION_POS";
  				   CH44 = "DIRECTION_DILATATION_NEG";
				   CH54 = "DIRECTION_DILATATION_OSCILLATIONS";
  				   CH64 = "DIRECTION_RESONANCE";
				   TEST29 = 1;
				}else if(selected.equals("CHOISISSEZ"))
				{
  				   CH14 = "CHOISISSEZ";
				   CH24 = "DIRECTION_DILATATION_POS";
  				   CH34 = "DIRECTION_DILATATION_NEG";
  				   CH44 = "DIRECTION_DILATATION_OSCILLATIONS";
  				   CH54 = "DIRECTION_RESONANCE";
  				   CH64 = "DIRECTION_DILATATION_POS_NEG";
				   TEST29 = 0;
				}
			}
  			});

		if (EQ2_YES == 1)
		{
		 namesComboBox144.setVisible(true);
		 namesComboBox14.setVisible(false);
		}else
		{
		 namesComboBox144.setVisible(false);
		 namesComboBox14.setVisible(true);
		}
		panel2.add(namesComboBox144);
   		panel2.add(namesComboBox14);

		chut1 = new JCheckBox("UTULISATEUR");
		chut1.setBounds(450,125,150,25);
		chut1.addItemListener(new ItemListener(){
		public void itemStateChanged(ItemEvent e){
				if (e.getStateChange() == ItemEvent.DESELECTED)
				{	
  				namesComboBox14.setEnabled(true);				
 			        chut2.setVisible(false);
				TEST12 = 0;
				if (TEST11 == 1) chut2.setVisible(true);
 			        TEST19 = 0;
				} else
				{
				chut2.setVisible(true);
				namesComboBox14.setEnabled(false);				
				TEST12 = 1;
 			        TEST19 = 1;
				}
 				}	
  			});

		if(TEST12 == 1 && EQ2_YES == 0)
		{	
			chut1.setSelected(true);
		}		
		if(TEST12 == 0)
		{	
			chut1.setSelected(false);
		}		
		if (EQ2_YES == 1)
		{
		 chut1.setVisible(false);
		}else if (EQ2_YES == 0)
		{
		 chut1.setVisible(true);
		}
		panel2.add(chut1);

		label_periode = new JLabel("PERIODE");
		label_periode.setBounds(710,100,80,25);
		label_periode.setVisible(false);
		panel2.add(label_periode);
		
		MAJ_PERIODE =  new JTextField();
		MAJ_PERIODE.setBounds(710,125,80,25);

		Font police = new Font("Arial", Font.BOLD, 14);
		MAJ_PERIODE.setFont(police);
		MAJ_PERIODE.setPreferredSize(new Dimension(80, 25));
		MAJ_PERIODE.setForeground(Color.BLUE);
					
		MAJ_PERIODE.setText(""+MAJPERIODE);

		MAJ_PERIODE.setVisible(false);
		panel2.add(MAJ_PERIODE);

		chut2 = new JCheckBox("MISE A JOUR");
		chut2.setBounds(600,125,110,25);
		chut2.addItemListener(new ItemListener(){
		public void itemStateChanged(ItemEvent e){
				if (e.getStateChange() == ItemEvent.DESELECTED)
				{	
				label_periode.setVisible(false);
				MAJ_PERIODE.setVisible(false);
				TEST16 = 0;
				} else
				{
				label_periode.setVisible(true);
				MAJ_PERIODE.setVisible(true);
				TEST16 = 1;
				}
 				}	
  			});
		

		if(TEST16 == 1)
		{	
			label_periode.setVisible(true);
			MAJ_PERIODE.setVisible(true);
			chut2.setSelected(true);
		}		
		if(TEST16 == 0)
		{	
			label_periode.setVisible(false);
			MAJ_PERIODE.setVisible(false);
			chut2.setSelected(false);
		}		


		if ((TEST11 == 1 || TEST12 == 1) && TEST10 == 0)
		{
		chut2.setVisible(true);
		MAJ_PERIODE.setVisible(true);
		label_periode.setVisible(true);
		}else
		{
		MAJ_PERIODE.setVisible(false);
		label_periode.setVisible(false);
		chut2.setVisible(false);
		}
		panel2.add(chut2);



		JCheckBox eq2 = new JCheckBox("EQUATION DE LAPLACIEN ( PARTIE POTENTIELLE - BULLE ABSTRAITE )");
		eq2.setBounds(20,185,500,25);
		eq2.addItemListener(new ItemListener(){	
		public void itemStateChanged(ItemEvent e){
				if (e.getStateChange() == ItemEvent.DESELECTED)
				{	
					EQ2_YES = 0;
					chut1.setVisible(true);
					chut2.setVisible(true);
					MAJ_PERIODE.setVisible(true);
					label_periode.setVisible(true);
 				        namesComboBox14.setVisible(true);
 				        namesComboBox144.setVisible(false);
					TEST10 = 0;
				} else
				{
					chut1.setVisible(false);
					chut2.setVisible(false);
					MAJ_PERIODE.setVisible(false);
					label_periode.setVisible(false);
  				        namesComboBox144.setVisible(true);
					namesComboBox14.setVisible(false);
					TEST10 = 1;
					EQ2_YES = 1;
				}
 				}	
  			});

		if(TEST10 == 1)
		{
		eq2.setSelected(true);
		}

		panel2.add(eq2);

		JCheckBox eq3 = new JCheckBox("EQUATION DE TEMPERATURE");
		eq3.setBounds(20,225,400,25);
		eq3.addItemListener(new ItemListener(){
		public void itemStateChanged(ItemEvent e){
				if (e.getStateChange() == ItemEvent.DESELECTED)
				{	
				} else
				{
				}
 				}	
  			});
		eq3.setEnabled(false);
		panel2.add(eq3);

		}	
										
		{
		JButton bOk = new JButton();
		bOk.setText("OK");
		bOk.setBounds(100, 330, 100, 30);
		bOk.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			if((TEST29 == 1 && EQ2_YES == 1) || (TEST19 == 1 && EQ2_YES == 0) )
			{
			bmeth.setEnabled(true);
			bposttrait.setEnabled(true);
   			MAJPERIODE=Double.parseDouble(MAJ_PERIODE.getText());
			OK5 = 1;
			if(TEST6 == 1)
			{
			if (OK1 == 1 && OK3 == 1 && OK4 == 1 && OK2 == 1 && OK6 == 1 && OK7 == 1){
   				bgenejdd.setEnabled(true);
 			}
			}
			if(TEST6 == 2)
			{
			if (OK1 == 1 && OK3 == 1 && OK4 == 1 && OK2 == 1 && OK7 == 1){
   				bgenejdd.setEnabled(true);
 			}
			}
			fsys.dispose();
			}
				}
			});
		panel2.add(bOk);
		}

		{
		JButton bCancel = new JButton();
		bCancel.setText("ANNULER");
		bCancel.setBounds(300, 330, 100, 30);
		bCancel.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			fsys.dispose();
				}
			});
		panel2.add(bCancel);
		}
				
				

		fsys.setVisible(true);
		fsys.setIconImage(new ImageIcon(getClass().getClassLoader().getResource("about.png")).getImage());
		fsys.setContentPane(panel2);
	}
}


public class FrameAmr
{
	private JFrame famr;
	public FrameAmr(){
		famr = new JFrame();
		famr.setSize(600, 730);
		//Nous allons maintenant dire à notre objet de se positionner au centre
		famr.setLocationRelativeTo(null);
		//donner un nom � la frame
		famr.setTitle("ADAPTATION AUTOMATIQUE DE MAILLAGE");	
		//on creer un container:
		JPanel panel2 = new JPanel();
		panel2.setLayout(null);
				
		JLabel label_amr = new JLabel("--- ADAPTATION AUTOMATIQUE DE MAILLAGE ---");
		label_amr.setBounds(125,10,350,25);
		panel2.add(label_amr);
		
		{
		JLabel label_met = new JLabel("METHODE AMR :");
		label_met.setBounds(20,50,150,25);
		panel2.add(label_met);

 		ButtonGroup bgroup115 = new ButtonGroup();
		JRadioButton cb115 = new JRadioButton("x__y__z - x__y__z");
		cb115.setBounds(150,50,140,25);
		cb115.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e){
				TEST20 = 1;
				}	
			});
		bgroup115.add(cb115);
		panel2.add(cb115);				

		JRadioButton cb125 = new JRadioButton("x__x - y__y - z__z");
		cb125.setBounds(340,50,140,25);
		cb125.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e){
				TEST20 = 2;
				}	
			});
		bgroup115.add(cb125);
		panel2.add(cb125);				

		if (TEST20 == 1)
		{
		cb115.setSelected(true);	
		}
	
		if (TEST20 == 2)
		{
		cb125.setSelected(true);	
		}
		JLabel label_type = new JLabel("TYPE AMR :");
		label_type.setBounds(20,90,150,25);
		panel2.add(label_type);

 		ButtonGroup bgroup135 = new ButtonGroup();
		JRadioButton cb135 = new JRadioButton("LOCAL");
		cb135.setBounds(150,90,140,25);
		cb135.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e){
				TEST21 = 1;
				}	
			});
		bgroup135.add(cb135);
		panel2.add(cb135);				

		JRadioButton cb145 = new JRadioButton("GLOBAL");
		cb145.setBounds(340,90,140,25);
		cb145.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e){
				TEST21 = 2;
				}	
			});
		bgroup135.add(cb145);
		panel2.add(cb145);				

		if(TEST21 == 1)
		{
		cb135.setSelected(true);	
		}
		if(TEST21 == 2)
		{
		cb145.setSelected(true);	
		}
		JLabel label_level = new JLabel("NOMBRE DE NIVEAU MAX :");
		label_level.setBounds(20,130,250,25);
		panel2.add(label_level);

		L_MAX =  new JTextField();
		L_MAX.setBounds(300,130,50,25);

		//Mise en forme champs de texte 2ieme conversion:
		Font police = new Font("Arial", Font.BOLD, 14);
		L_MAX.setFont(police);
		L_MAX.setPreferredSize(new Dimension(50, 30));
		L_MAX.setForeground(Color.BLUE);

		L_MAX.setText(""+LMAX);
					
		panel2.add(L_MAX);


		JLabel label_isf = new JLabel("NOMBRE DE MAILLES DE SECURITE :");
		label_isf.setBounds(20,170,250,25);
		panel2.add(label_isf);

		ISF_MAX =  new JTextField();
		ISF_MAX.setBounds(300,170,50,25);

		//Mise en forme champs de texte 2ieme conversion:
		ISF_MAX.setFont(police);
		ISF_MAX.setPreferredSize(new Dimension(50, 30));
		ISF_MAX.setForeground(Color.BLUE);

		ISF_MAX.setText(""+ISFMAX);
					
		panel2.add(ISF_MAX);

		JLabel jLabelrc = new JLabel();
		jLabelrc.setText("COEFICIENTS DE RAFINEMENT POUR CHAQUE DIRECTION:");
		jLabelrc.setBounds(20, 210, 430, 25);
		panel2.add(jLabelrc);
		RC_X = new JTextField();
		RC_Y = new JTextField();     
		RC_Z = new JTextField();     

		JLabel jLabelrcX = new JLabel();
		jLabelrcX.setText("X");
		jLabelrcX.setBounds(455, 210, 25, 25);
		panel2.add(jLabelrcX);

		JLabel jLabelrcY = new JLabel();
		jLabelrcY.setText("Y");
		jLabelrcY.setBounds(455, 260, 25, 25);
		panel2.add(jLabelrcY);

		jLabelrcZ = new JLabel();
		jLabelrcZ.setText("Z");
		jLabelrcZ.setBounds(455, 310, 25, 25);
		if (RC_Z_YES == 1)
		{
  			jLabelrcZ.setVisible(false);
		}else
		{
  			jLabelrcZ.setVisible(true);
		}
		panel2.add(jLabelrcZ);

		RC_X.setBounds(480,210,80,25);
		RC_Y.setBounds(480,260,80,25);
		RC_Z.setBounds(480,310,80,25);
		RC_X.setFont(police);
		RC_X.setPreferredSize(new Dimension(80, 25));
		RC_X.setForeground(Color.BLUE);

		RC_Y.setFont(police);
		RC_Y.setPreferredSize(new Dimension(80, 25));
		RC_Y.setForeground(Color.BLUE);

		RC_Z.setFont(police);
		RC_Z.setPreferredSize(new Dimension(80, 25));
		RC_Z.setForeground(Color.BLUE);

		RC_X.setText(""+RCX);
		RC_Y.setText(""+RCY);
		RC_Z.setText(""+RCZ);

		panel2.add(RC_X);
		panel2.add(RC_Y);
		if (RC_Z_YES == 1)
		{
  			RC_Z.setVisible(false);
		}else
		{
  		 	RC_Z.setVisible(true);
		}
		panel2.add(RC_Z);


		JLabel jLabeldimcut = new JLabel();
		jLabeldimcut.setText("NOMBRE DE NOEUDS MAX PAR PATCHE POUR CHAQUE DIRECTION:");
		jLabeldimcut.setBounds(20, 350, 430, 25);
		panel2.add(jLabeldimcut);
		DIMCUT_X = new JTextField();
		DIMCUT_Y = new JTextField();     
		DIMCUT_Z = new JTextField();     

		JLabel jLabeldimcutX = new JLabel();
		jLabeldimcutX.setText("X");
		jLabeldimcutX.setBounds(455, 350, 25, 25);
		panel2.add(jLabeldimcutX);

		JLabel jLabeldimcutY = new JLabel();
		jLabeldimcutY.setText("Y");
		jLabeldimcutY.setBounds(455, 400, 25, 25);
		panel2.add(jLabeldimcutY);

		jLabeldimcutZ = new JLabel();
		jLabeldimcutZ.setText("Z");
		jLabeldimcutZ.setBounds(455, 450, 25, 25);
		if (DIMCUT_Z_YES == 1)
		{
  			jLabeldimcutZ.setVisible(false);
		}else
		{
  			jLabeldimcutZ.setVisible(true);
		}
		panel2.add(jLabeldimcutZ);

		DIMCUT_X.setBounds(480,350,80,25);
		DIMCUT_Y.setBounds(480,400,80,25);
		DIMCUT_Z.setBounds(480,450,80,25);
		DIMCUT_X.setFont(police);
		DIMCUT_X.setPreferredSize(new Dimension(80, 25));
		DIMCUT_X.setForeground(Color.BLUE);

		DIMCUT_Y.setFont(police);
		DIMCUT_Y.setPreferredSize(new Dimension(80, 25));
		DIMCUT_Y.setForeground(Color.BLUE);

		DIMCUT_Z.setFont(police);
		DIMCUT_Z.setPreferredSize(new Dimension(80, 25));
		DIMCUT_Z.setForeground(Color.BLUE);

		DIMCUT_X.setText(""+DIMCUTX);
		DIMCUT_Y.setText(""+DIMCUTY);
		DIMCUT_Z.setText(""+DIMCUTZ);

		panel2.add(DIMCUT_X);
		panel2.add(DIMCUT_Y);
		if (DIMCUT_Z_YES == 1)
		{
  			DIMCUT_Z.setVisible(false);
		}else
		{
  		 	DIMCUT_Z.setVisible(true);
		}
		panel2.add(DIMCUT_Z);
										

		JLabel label_sen1 = new JLabel("CRITERE DE RAFINEMENT :");
		label_sen1.setBounds(20,490,180,25);
		panel2.add(label_sen1);

 		ButtonGroup bgroup155 = new ButtonGroup();
		JRadioButton cb155 = new JRadioButton("PAR DEFAUT");
		cb155.setBounds(200,490,110,25);
		cb155.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e){
				TEST30 = 1;
				TOL_SEN.setVisible(true);
				}	
			});
		bgroup155.add(cb155);
		panel2.add(cb155);				

		JRadioButton cb165 = new JRadioButton("UTULISATEUR");
		cb165.setBounds(450,490,140,25);
		cb165.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent e){
				TEST30 = 2;
				TOL_SEN.setVisible(false);
				}	
			});
		bgroup155.add(cb165);
		panel2.add(cb165);				

		TOL_SEN =  new JTextField();
		TOL_SEN.setBounds(320,490,80,25);

		//Mise en forme champs de texte 2ieme conversion:
		TOL_SEN.setFont(police);
		TOL_SEN.setPreferredSize(new Dimension(80, 30));
		TOL_SEN.setForeground(Color.BLUE);

		TOL_SEN.setText(""+TOLSEN);

		panel2.add(TOL_SEN);

		TOL_SEN.setVisible(false);
		panel2.add(TOL_SEN);
		if(TEST30 == 1)
		{
			TOL_SEN.setVisible(true);
		}
		if(TEST30 == 2)
		{
			TOL_SEN.setVisible(false);
		}

		if(TEST30 == 1)
		{
		cb155.setSelected(true);	
		}
		if(TEST30 == 2)
		{
		cb165.setSelected(true);	
		}

		JLabel label_grcl = new JLabel("TOLERANCE POUR L'ALGORITHME DE GROUPING/CLUSTERING :");
		label_grcl.setBounds(20,530,400,25);
		panel2.add(label_grcl);

		TOL_GRCL =  new JTextField();
		TOL_GRCL.setBounds(450,530,80,25);

		//Mise en forme champs de texte 2ieme conversion:
		TOL_GRCL.setFont(police);
		TOL_GRCL.setPreferredSize(new Dimension(80, 30));
		TOL_GRCL.setForeground(Color.BLUE);

		TOL_GRCL.setText(""+TOLGRCL);

		panel2.add(TOL_GRCL);

		JLabel label_famr = new JLabel("FREQUENCE DE GENERATION DE MAILLAGE (ITERATIONS) :");
		label_famr.setBounds(20,570,400,25);
		panel2.add(label_famr);

		F_AMR =  new JTextField();
		F_AMR.setBounds(450,570,80,25);

		//Mise en forme champs de texte 2ieme conversion:
		F_AMR.setFont(police);
		F_AMR.setPreferredSize(new Dimension(80, 30));
		F_AMR.setForeground(Color.BLUE);

		F_AMR.setText(""+FAMR);

		panel2.add(F_AMR);

		}	
										



		{
		JButton bOk = new JButton();
		bOk.setText("OK");
		bOk.setBounds(100, 660, 100, 30);
		bOk.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			if (TEST20 >= 1 && TEST21 >= 1 && TEST30 >= 1){
   			LMAX=Integer.parseInt(L_MAX.getText());
   			ISFMAX=Integer.parseInt(ISF_MAX.getText());
   			RCX=Integer.parseInt(RC_X.getText());
   			RCY=Integer.parseInt(RC_Y.getText());
   			RCZ=Integer.parseInt(RC_Z.getText());
   			DIMCUTX=Integer.parseInt(DIMCUT_X.getText());
   			DIMCUTY=Integer.parseInt(DIMCUT_Y.getText());
   			DIMCUTZ=Integer.parseInt(DIMCUT_Z.getText());
			TOLSEN=Double.parseDouble(TOL_SEN.getText());
			TOLGRCL=Double.parseDouble(TOL_GRCL.getText());
			FAMR=Integer.parseInt(F_AMR.getText());
			OK6 = 1;
			if (OK1 == 1 && OK3 == 1 && OK4 == 1 && OK5 == 1 && OK2 == 1 && OK7 == 1){
   				bgenejdd.setEnabled(true);
 			}
			famr.dispose();
 			}
				}
			});
		panel2.add(bOk);
		}

		{
		JButton bCancel = new JButton();
		bCancel.setText("ANNULER");
		bCancel.setBounds(300, 660, 100, 30);
		bCancel.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			famr.dispose();
				}
			});
		panel2.add(bCancel);
		}
				
				

		famr.setVisible(true);
		famr.setIconImage(new ImageIcon(getClass().getClassLoader().getResource("about.png")).getImage());
		famr.setContentPane(panel2);
	}
}



public class FramePost
{
	private JFrame fpost;
	public FramePost(){
		fpost = new JFrame();
		fpost.setSize(600, 500);
		//Nous allons maintenant dire à notre objet de se positionner au centre
		fpost.setLocationRelativeTo(null);
		//donner un nom � la frame
		fpost.setTitle("POST-TRAITEMENT");	
		//on creer un container:
		JPanel panel2 = new JPanel();
		panel2.setLayout(null);
				
		JLabel label_par = new JLabel("--- VISUALISATION FORMAT PARAVIEW ---");
		label_par.setBounds(150,30,300,25);
		panel2.add(label_par);
		
		JLabel label_var = new JLabel(" VARIABLES ");
		label_var.setBounds(60,80,150,25);
		panel2.add(label_var);

		JLabel label_freq = new JLabel(" FREQUENCE DE SORTIE ");
		label_freq.setBounds(300,80,200,25);
		panel2.add(label_freq);

		{
		JCheckBox fracvol = new JCheckBox("FRACTION VOLUMIQUE");
		fracvol.setBounds(20,120,200,25);
		fracvol.addItemListener(new ItemListener(){
		public void itemStateChanged(ItemEvent e){
				if (e.getStateChange() == ItemEvent.DESELECTED)
				{
					FREQ_FRACVOL.setVisible(false);	
					TEST23 = 0;
				} else
				{
					FREQ_FRACVOL.setVisible(true);	
					TEST23 = 1;
				}
 				}	
  			});
		panel2.add(fracvol);

		FREQ_FRACVOL =  new JTextField();
		FREQ_FRACVOL.setBounds(320,120,80,25);

		//Mise en forme champs de texte 2ieme conversion:
		Font police = new Font("Arial", Font.BOLD, 14);
		FREQ_FRACVOL.setFont(police);
		FREQ_FRACVOL.setPreferredSize(new Dimension(80, 30));
		FREQ_FRACVOL.setForeground(Color.BLUE);

		FREQ_FRACVOL.setText(""+FREQFRACVOL);
 		FREQ_FRACVOL.setVisible(false);

		if (TEST23 == 1)
		{
		FREQ_FRACVOL.setVisible(true);
		fracvol.setSelected(true);	
		}	
		panel2.add(FREQ_FRACVOL);

		JCheckBox vitesse = new JCheckBox("VITESSE");
		vitesse.setBounds(20,160,200,25);
		vitesse.addItemListener(new ItemListener(){
		public void itemStateChanged(ItemEvent e){
				if (e.getStateChange() == ItemEvent.DESELECTED)
				{
					FREQ_VITESSE.setVisible(false);	
				} else
				{
					FREQ_VITESSE.setVisible(true);	
				}
 				}	
  			});
		vitesse.setEnabled(false);
		panel2.add(vitesse);

		FREQ_VITESSE =  new JTextField();
		FREQ_VITESSE.setBounds(320,160,80,25);

		//Mise en forme champs de texte 2ieme conversion:
		FREQ_VITESSE.setFont(police);
		FREQ_VITESSE.setPreferredSize(new Dimension(80, 30));
		FREQ_VITESSE.setForeground(Color.BLUE);

		FREQ_VITESSE.setText(""+FREQVITESSE);
		FREQ_VITESSE.setVisible(false);
		panel2.add(FREQ_VITESSE);

		}

		JLabel label_lis = new JLabel("--- VISUALISATION FORMAT LISTING ---");
		label_lis.setBounds(150,250,300,25);
		panel2.add(label_lis);



/*		{

		eq1.setBounds(20,80,400,25);
		panel2.add(eq1);
			

		}	*/
										
		{
		JButton bOk = new JButton();
		bOk.setText("OK");
		bOk.setBounds(100, 430, 100, 30);
		bOk.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			if(TEST23 >= 1)
			{
			OK7 = 1;
			FREQFRACVOL=Integer.parseInt(FREQ_FRACVOL.getText());
			if(TEST6 == 1)
			{
			if (OK1 == 1 && OK3 == 1 && OK4 == 1 && OK5 == 1 && OK6 == 1 && OK2 == 1){
   				bgenejdd.setEnabled(true);
 			}
			}
			if(TEST6 == 2)
			{
			if (OK1 == 1 && OK3 == 1 && OK4 == 1 && OK5 == 1 && OK2 == 1){
   				bgenejdd.setEnabled(true);
 			}
			}
			fpost.dispose();
			}
				}
			});
		panel2.add(bOk);
		}

		{
		JButton bCancel = new JButton();
		bCancel.setText("ANNULER");
		bCancel.setBounds(300, 430, 100, 30);
		bCancel.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			fpost.dispose();
				}
			});
		panel2.add(bCancel);
		}
				
				

		fpost.setVisible(true);
		fpost.setIconImage(new ImageIcon(getClass().getClassLoader().getResource("about.png")).getImage());
		fpost.setContentPane(panel2);
	}
}







// private void ouvrirFichier() {
// JFileChooser jfc = new JFileChooser();
// 
// int resultat = jfc.showOpenDialog(this);
// if (resultat == JFileChooser.APPROVE_OPTION) {
// fichierCourant = jfc.getSelectedFile();
// lireFichier(fichierCourant);
// }
// else {
// fichierCourant = null;
// }
// }


// private void enregistrerFichier() {
// if (fichierCourant == null) { afficheMessage("Aucun emplacement n'a été spécifier", "Information");
// ecrireFichier( new File("Nouveau Document Texte.txt"), " " );
// }
// else
// ecrireFichier(fichierCourant);
// }


// private void enregistrerFichierSous() {
// JFileChooser jfc = new JFileChooser();
// jfc.setDialogTitle("Enregistrer sous...");
// jfc.setApproveButtonText("Enregistrer");
// jfc.setApproveButtonToolTipText("Enregistrer le fichier à cet emplacement");
// 
// int resultat = jfc.showOpenDialog(this);
// if (resultat == JFileChooser.APPROVE_OPTION)
// ecrireFichier(jfc.getSelectedFile()," ");
// }


private void lireFichier(File fichier) {
String ligne;
StringBuffer buf = new StringBuffer();
try {
tA.setText("");
BufferedReader in = new BufferedReader(
new FileReader(fichier) );
while ((ligne = in.readLine()) != null)
buf.append(ligne + (char) '\n');
tA.setText(buf.toString());
}
catch(FileNotFoundException e) {}
catch(IOException e) {}
}


private void afficheMessage(String message, String titre) {
JOptionPane.showMessageDialog(null, message,
titre, JOptionPane.INFORMATION_MESSAGE);
} 
	
}


