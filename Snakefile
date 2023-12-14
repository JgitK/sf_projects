rule get_data:
    input:
        script = "code/get_data.bash"
    output:
        archive = "data/raw/sf_incidents_new.csv"
    shell:
        """
        {input.script}
        """

rule run_shiny:
    input:
        script = "shiny_app/global.R"
        archive = "data/raw/sf_incidents_new.csv"
    output:
        
