#!/usr/bin/env ruby

require 'bundler/setup'
require 'parallel'
require 'bgs'
require 'csv'

bgs = BGS::Services.new(
  env: 'webapps',
  client_ip: '127.0.0.1',
  client_station_id: '283',
  client_username: 'CSFLOW',
  application: 'CASEFLOW',
  ssl_cert_file: ENV['BGS_CERT'],
  ssl_cert_key_file: ENV['BGS_KEY'],
  ssl_ca_cert: ENV['BGS_CA_CERT']
)

ep_codes = %w(
  170APPACT
  170APPACTPMC
  170PGAMC
  170RMD
  170RMDAMC
  170RMDPMC
  172GRANT
  172BVAG
  172BVAGPMC
  400CORRC
  400CORRCPMC
  930RC
  930RCPMC
)

col_names = %w(
  BFCORLID
  benefit_claim_id
  claim_receive_date
  claim_type_code
  claim_type_name
  claimant_first_name
  claimant_last_name
  claimant_middle_name
  claimant_suffix
  end_product_type_code
  last_action_date
  organization_name
  organization_title_type_name
  payee_type_code
  person_or_organization_indicator
  program_type_code
  status_type_code
  journal_date
  journal_object_id
  journal_station
  journal_status_type_code
  journal_user_id
)

CSV.open('claims.csv', 'w') do |csv|
  csv << col_names

  Parallel.each(ARGF, in_threads: 16, progress: 'Loading cases') do |bfcorlid|
    bfcorlid.strip!
    vbms_id = bfcorlid[0...-1].rjust(8, '0')
    begin
      bgs.claims.find_by_vbms_file_number(vbms_id)
         .select { |claim| ep_codes.include? claim[:claim_type_code] }
         .each do |claim|
           detail = bgs.claims.find_claim_detail_by_id claim[:benefit_claim_id]
           journals = detail[:life_cycle_record][:life_cycle_records]
           first_journal = journals.is_a?(Hash) ? journals : journals.first
           values = [bfcorlid] +
                    claim.values +
                    [first_journal[:journal_date],
                     first_journal[:journal_object_id],
                     first_journal[:journal_station],
                     first_journal[:journal_status_type_code],
                     first_journal[:journal_user_id]]
           csv << values
         end
    rescue => e
      puts "Problem loading case #{bfcorlid}"
      puts e
    end
  end
end
