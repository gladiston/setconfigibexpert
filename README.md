# setconfigibexpert
Restaura as configurações do IBExpert num único clique

Se você usa o banco de dados FirebirdSQL e usa o aplicativo IBExpert para gerenciá-lo, talvez este programa seja para você. Mas antes de usar este programa você deve seguir este passo a passo: 
(1) Você deve configurar o IBExpert exatametne como deseja, com todos ajustes, conexões, pastas...tudo como deveria ser. 
(2) Após a configuração inicial e ajustes, vá no menu Options|Envoroment Options e selecionar a guia "IBExpert User Database" e então em "User Database Connection String" definir um banco de dados local será onde o IBExpert guardará todos esses ajustes, por exemplo, C:\MyDatabases\IbExpert_FB3.fdb. 
(3) Ainda na guia "IBExpert User Database", caso deseje também que seus projetos de banco de dados sejam salvos então marcar a opção "Store Project View Data in User Database" 
(4) Ainda na guia "IBExpert User Database" clique em "OK" e o IBExpert criará este banco com todas as suas definições atuais e a armazenará neste banco. Como dito, o IBExpert armazenará toda sua configuração, pastas, conexões, históricos,... nele e a atualizará a cada nova interatividade com bancos e/ou projetos novos.

Uma vez que tem um arquivo de dados de configuração do IBExpert, você pode levar o IBExpert de um local para outro e todos os seus ajustes estarão onde você for, mas terá de repetir os passos 2 até o 4. É aqui que o programa "setconfigibexpert" entra, ele repetirá os passos 2 a 4 por você, basta dizer onde está este banco de dados que guarda as definições do IBExpert e ele fará o ajuste necessário de que precisa, economizando tempo.
